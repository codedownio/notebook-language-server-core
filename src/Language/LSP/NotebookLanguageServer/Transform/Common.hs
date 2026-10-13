{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LSP.NotebookLanguageServer.Transform.Common where

import Control.Applicative
import Control.Lens hiding (List)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
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

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the cell; drop individual
-- edits that aren't.
untransformInlayHint :: Transformer tx => tx -> InlayHint -> Maybe InlayHint
untransformInlayHint tx hint =
  hint
    & traverseOf position (untransformPosition tx)
    & fmap (over (textEdits . _Just) (mapMaybe (untransformRanged tx)))

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
      | u == newUri = (origUri, ) <$> traverse (untransformRanged tx) edits
      | otherwise = Nothing

    untransformDocumentChange (InL textDocumentEdit) = InL <$> untransformTextDocumentEdit textDocumentEdit
    untransformDocumentChange (InR _fileOperation) = Nothing

    untransformTextDocumentEdit (TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u _version) edits)
      | u == newUri = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier origUri (InR Null))
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
