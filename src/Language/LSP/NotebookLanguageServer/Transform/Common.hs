{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LSP.NotebookLanguageServer.Transform.Common where

import Control.Applicative
import Control.Lens hiding (List, children)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Transformer
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens as Lens


-- * Transform

transformRange :: Transformer tx => tx -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (Transformer tx, HasRange a Range) => tx -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition tx)
  >>= traverseOf (range . end) (transformPosition tx)

-- * Untransform

untransformRange :: Transformer tx => tx -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (Transformer tx, HasRange a Range) => tx -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition tx)
  >>= traverseOf (range . end) (untransformPosition tx)

untransformRangedMaybe :: (Transformer tx, HasRange a (Maybe Range)) => tx -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition tx)
  >>= traverseOf (range . _Just . end) (untransformPosition tx)

-- | A document outline, back in the notebook's terms.
--
-- The tree is the projection's, so its upper levels are whatever scaffolding the language
-- needed -- jnls wraps every cell in a @class __Cells@, gnls in a @func _notebookExec@.
-- Those nodes span synthetic lines and don't untransform, but the cell's own symbols hang
-- off them, so a node we can't place is replaced by its children rather than dropped with
-- them. A node the language asks us to ignore goes with its children, which is the point of
-- ignoring it.
untransformDocumentSymbols :: Transformer tx => (Text -> Bool) -> tx -> [DocumentSymbol] -> [DocumentSymbol]
untransformDocumentSymbols ignoreSymbol tx = concatMap go
  where
    go sym
      | ignoreSymbol (sym ^. name) = []
      | otherwise = case placed of
          Nothing -> kids
          Just sym' -> [sym' & set children (if null kids then Nothing else Just kids)]
      where
        placed = untransformRanged tx sym >>= traverseOf selectionRange (untransformRange tx)
        kids = untransformDocumentSymbols ignoreSymbol tx (fromMaybe [] (sym ^. children))

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the cell; drop individual
-- edits that aren't.
untransformInlayHint :: Transformer tx => tx -> InlayHint -> Maybe InlayHint
untransformInlayHint tx hint =
  hint
    & traverseOf position (untransformPosition tx)
    & fmap (over (textEdits . _Just) (mapMaybe (untransformRanged tx)))

-- | A hierarchy item the client is handing back, in the shadow document's terms. An item from
-- some other file goes out as it came in.
transformHierarchyItem :: (
  Transformer tx, HasUri a Uri, HasRange a Range, HasSelectionRange a Range
  ) => DocumentState tx -> a -> Maybe a
transformHierarchyItem (DocumentState {transformer=tx, origUri, newUri}) x
  | sameDocument (x ^. uri) origUri = do
      range' <- transformRange tx (x ^. range)
      selectionRange' <- transformRange tx (x ^. selectionRange)
      return $ x & set uri newUri & set range range' & set selectionRange selectionRange'
  | otherwise = Just x

-- | A hierarchy item the server answered with, back in the notebook's terms.
untransformHierarchyItem :: (
  Transformer tx, HasUri a Uri, HasRange a Range, HasSelectionRange a Range
  ) => DocumentState tx -> a -> Maybe a
untransformHierarchyItem (DocumentState {transformer=tx, origUri, newUri}) x
  | sameDocument (x ^. uri) newUri = do
      range' <- untransformRange tx (x ^. range)
      selectionRange' <- untransformRange tx (x ^. selectionRange)
      return $ x & set uri origUri & set range range' & set selectionRange selectionRange'
  | otherwise = Just x

-- | An incoming call names the caller and where inside it the calls are, so those ranges belong
-- to the caller's own document.
untransformIncomingCall :: Transformer tx => DocumentState tx -> CallHierarchyIncomingCall -> Maybe CallHierarchyIncomingCall
untransformIncomingCall ds@(DocumentState {transformer=tx, newUri}) call = do
  from' <- untransformHierarchyItem ds (call ^. Lens.from)
  let ranges | sameDocument (call ^. Lens.from . uri) newUri = mapMaybe (untransformRange tx) (call ^. fromRanges)
             | otherwise = call ^. fromRanges
  return $ call & set Lens.from from' & set fromRanges ranges

-- | An outgoing call's ranges are where the call is written, which is the document the client
-- asked about.
untransformOutgoingCall :: Transformer tx => DocumentState tx -> CallHierarchyOutgoingCall -> Maybe CallHierarchyOutgoingCall
untransformOutgoingCall ds@(DocumentState {transformer=tx}) call = do
  to' <- untransformHierarchyItem ds (call ^. Lens.to)
  return $ call & set Lens.to to' & set fromRanges (mapMaybe (untransformRange tx) (call ^. fromRanges))

-- | A location the server answered with, back in the notebook's terms. Anything outside the
-- document we shadow is left alone: it's a real file the client can open for itself.
untransformLocation :: Transformer tx => DocumentState tx -> Location -> Maybe Location
untransformLocation (DocumentState {transformer=tx, origUri, newUri}) loc
  | sameDocument (loc ^. uri) newUri = set uri origUri <$> untransformRanged tx loc
  | otherwise = Just loc

untransformLocationLink :: Transformer tx => DocumentState tx -> LocationLink -> Maybe LocationLink
untransformLocationLink (DocumentState {transformer=tx, origUri, newUri}) link
  | sameDocument (link ^. targetUri) newUri = do
      targetRange' <- untransformRange tx (link ^. targetRange)
      targetSelectionRange' <- untransformRange tx (link ^. targetSelectionRange)
      originSelectionRange' <- traverse (untransformRange tx) (link ^. originSelectionRange)
      return $ link
        & set targetUri origUri
        & set targetRange targetRange'
        & set targetSelectionRange targetSelectionRange'
        & set originSelectionRange originSelectionRange'
  | otherwise = Just link

-- | A selection range is a chain, each one naming the wider range that contains it. The widest
-- are the wrapper the shadow document puts around the cells, which the notebook has nothing to
-- correspond to, so the chain is cut where it leaves the cell rather than thrown away: expanding
-- the selection should stop at the cell, not stop working.
untransformSelectionRange :: Transformer tx => tx -> SelectionRange -> Maybe SelectionRange
untransformSelectionRange tx (SelectionRange r parent) = do
  r' <- untransformRange tx r
  return $ SelectionRange r' (parent >>= untransformSelectionRange tx)

-- | Map a workspace edit from the shadow document back to the notebook.
--
-- 'Nothing' when the edit can't be honoured from the notebook's point of view: it names a
-- document that isn't this notebook's shadow document, it creates, renames or deletes a file,
-- or one of its ranges doesn't map back into a cell. Applying half of such an edit would be
-- worse than refusing it.
untransformWorkspaceEdit :: Transformer tx => DocumentState tx -> WorkspaceEdit -> Maybe WorkspaceEdit
untransformWorkspaceEdit (DocumentState {transformer=tx, origUri, newUri}) (WorkspaceEdit changes documentChanges changeAnnotations) =
  WorkspaceEdit
    <$> traverse untransformChanges changes
    <*> traverse (traverse untransformDocumentChange) documentChanges
    <*> pure changeAnnotations
  where
    untransformChanges = fmap M.fromList . traverse untransformEntry . M.toList

    untransformEntry (u, edits)
      | sameDocument u newUri = (origUri, ) <$> traverse (untransformRanged tx) edits
      | otherwise = Nothing

    untransformDocumentChange (InL textDocumentEdit) = InL <$> untransformTextDocumentEdit textDocumentEdit
    untransformDocumentChange (InR _fileOperation) = Nothing

    untransformTextDocumentEdit (TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u _version) edits)
      | sameDocument u newUri = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier origUri (InR Null))
                        <$> traverse untransformEdit edits
      | otherwise = Nothing

    untransformEdit (InL textEdit) = InL <$> untransformRanged tx textEdit
    untransformEdit (InR annotatedTextEdit) = InR <$> untransformRanged tx annotatedTextEdit

-- | The document a workspace edit names, if it names one at all. Used to find the notebook a
-- resolved code action belongs to, since codeAction/resolve doesn't carry a text document.
workspaceEditUri :: WorkspaceEdit -> Maybe Uri
workspaceEditUri (WorkspaceEdit changes documentChanges _changeAnnotations) =
  listToMaybe (concatMap documentChangeUris (fromMaybe [] documentChanges))
  <|> listToMaybe (M.keys (fromMaybe mempty changes))
  where
    documentChangeUris (InL (TextDocumentEdit ident _edits)) = [ident ^. uri]
    documentChangeUris (InR _fileOperation) = []

-- * Code lens data

-- | codeLens/resolve carries no text document, so the notebook a lens came from rides along in
-- the lens's @data@ next to whatever the server put there.
stampLensData :: Uri -> CodeLens -> CodeLens
stampLensData u lens = lens & set data_ (Just (A.object ["nls_uri" A..= u, "nls_data" A..= (lens ^. data_)]))

unstampLensData :: CodeLens -> Maybe (Uri, CodeLens)
unstampLensData lens = case lens ^. data_ of
  Just (A.Object o) | Just (A.Success u) <- A.fromJSON <$> KM.lookup "nls_uri" o ->
    Just (u, set data_ (KM.lookup "nls_data" o >>= nonNull) lens)
  _ -> Nothing
  where
    nonNull A.Null = Nothing
    nonNull v = Just v

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
