{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.NotebookLanguageServer.Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import Data.String.Interpolate
import Data.Time
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Formatting
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover
import Language.LSP.NotebookLanguageServer.Transform.Util
import UnliftIO.MVar
import Language.LSP.Transformer


type ServerRspMethod m = SMethod (m :: Method 'ClientToServer 'Request)

transformServerRsp :: (
  TransformerMonad tx n, Transformer tx, HasJSON (TResponseMessage m)
  ) => NotebookConfig tx -> ServerRspMethod m -> MessageParams m -> TResponseMessage m -> n (TResponseMessage m)
transformServerRsp config meth initialParams msg = case msg ^. result of
  Left _err -> return msg
  Right ret -> do
    start <- liftIO getCurrentTime
    p' <- transformServerRsp' config meth initialParams ret
    stop <- liftIO getCurrentTime
    let msg' = set result (Right p') msg
    when (msg' /= msg) $ logDebugN [i|Transforming server rsp #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
    return $ set result (Right p') msg

type ResponseResult m = Either ResponseError (MessageResult m)

transformServerRsp' :: (TransformerMonad tx n, Transformer tx) => NotebookConfig tx -> ServerRspMethod m -> MessageParams m -> MessageResult m -> n (MessageResult m)

transformServerRsp' _ SMethod_Initialize _initialParams result = do
  initializeResultVar <- asks transformerInitializeResult
  modifyMVar_ initializeResultVar (\_ -> return $ Just result)
  return result

transformServerRsp' config SMethod_TextDocumentCompletion initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    let fixupCompletionEdit Nothing = Nothing
        fixupCompletionEdit (Just (InL edit)) = InL <$> (untransformRanged tx edit)
        fixupCompletionEdit (Just (InR (InsertReplaceEdit newText insert replace))) =
          InR <$> (InsertReplaceEdit newText <$> untransformRanged tx insert <*> untransformRanged tx replace)

    let fixupItem :: CompletionItem -> CompletionItem
        fixupItem item = item
         & over textEdit fixupCompletionEdit
         & over (additionalTextEdits . _Just) (mapMaybe (untransformRanged tx))

    case result of
      (InL items) -> return (InL (fmap fixupItem items))
      (InR (InL completionList)) -> return (InR (InL (completionList & over items (fmap fixupItem))))
      (InR (InR null)) -> return $ InR $ InR null

transformServerRsp' config SMethod_TextDocumentDocumentHighlight initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL highlights -> return $ InL $ mapMaybe (untransformRanged tx) highlights
      InR null -> return $ InR null

transformServerRsp' config SMethod_TextDocumentFormatting _initialParams result
  | not (notebookConfigHandlesFormatting config) = return result
transformServerRsp' config SMethod_TextDocumentFormatting initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    case result of
      InL edits -> return $ InL $ untransformFormattingEdits ds edits
      InR null -> return $ InR null

transformServerRsp' config SMethod_TextDocumentOnTypeFormatting initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL edits -> return $ InL $ mapMaybe (untransformRanged tx) edits
      InR null -> return $ InR null

transformServerRsp' config SMethod_TextDocumentInlayHint initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL hints -> return $ InL $ mapMaybe (untransformInlayHint tx) hints
      InR null -> return $ InR null

transformServerRsp' config SMethod_TextDocumentHover initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InR null -> return $ InR null
      InL hov -> do
        hov' <- fixupHoverText hov
        return $ case untransformRangedMaybe tx hov' of
          Just ret -> InL ret
          Nothing -> InR LSP.Null

transformServerRsp' config SMethod_TextDocumentDocumentSymbol initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL symbolInformations -> return $ InL (symbolInformations & filter (not . ignoreSymbol)
                                                                 & mapMaybe (traverseOf (location . range) (untransformRange tx)))
      InR (InL documentSymbols) ->
        return $ InR $ InL $ untransformDocumentSymbols (notebookConfigIgnoreSymbol config) tx documentSymbols
      InR (InR LSP.Null) -> return $ InR $ InR LSP.Null
  where
    ignoreSymbol :: HasName a Text => a -> Bool
    ignoreSymbol sym = notebookConfigIgnoreSymbol config (sym ^. name)

-- | The goto family answers with locations inside the shadow document. Without putting them
-- back the client is told to open a file it has never heard of.
transformServerRsp' config SMethod_TextDocumentDefinition initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return (untransformDefinitionResult ds result)
transformServerRsp' config SMethod_TextDocumentDeclaration initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return (untransformDeclarationResult ds result)
transformServerRsp' config SMethod_TextDocumentTypeDefinition initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return (untransformDefinitionResult ds result)
transformServerRsp' config SMethod_TextDocumentImplementation initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return (untransformDefinitionResult ds result)
transformServerRsp' config SMethod_TextDocumentReferences initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return (over _L (mapMaybe (untransformLocation ds)) result)

-- | Renaming answers with an edit over the shadow document; applying it to the notebook
-- without this would put the new name on the wrong lines.
transformServerRsp' config SMethod_TextDocumentRename initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    case result of
      InR null -> return $ InR null
      InL edit -> return $ case untransformWorkspaceEdit ds edit of
        Just edit' -> InL edit'
        Nothing -> InR LSP.Null

-- | prepareRename answers with the range the name occupies, which the editor highlights.
transformServerRsp' config SMethod_TextDocumentPrepareRename initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return (untransformPrepareRename tx result)

transformServerRsp' config SMethod_TextDocumentSelectionRange initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return (over _L (mapMaybe (untransformSelectionRange tx)) result)

-- | The token data is groups of five integers, the line relative to the token before it and the
-- start character relative to that token's start within a line. Moving them means decoding to
-- absolute positions, mapping, re-sorting and re-encoding: the encoding only reads back
-- correctly in position order.
transformServerRsp' config SMethod_TextDocumentSemanticTokensFull initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return (over (_L . Lens.data_) (untransformSemanticTokenData tx) result)
transformServerRsp' config SMethod_TextDocumentSemanticTokensRange initialParams result =
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return (over (_L . Lens.data_) (untransformSemanticTokenData tx) result)

transformServerRsp' _ SMethod_TextDocumentCodeAction _initialParams (InR null) = return $ InR null
transformServerRsp' config SMethod_TextDocumentCodeAction initialParams (InL result) = (InL <$>) $
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \ds ->
    return $ mapMaybe (untransformCodeActionOrCommand ds) result

-- | rust-analyzer fills in a code action's edit inline when it can and leaves it for resolve
-- when it can't, so both paths need untransforming. A resolve carries no text document, so the
-- notebook has to be found from the shadow uri the edit names.
transformServerRsp' _ SMethod_CodeActionResolve _initialParams result = case result ^. edit of
  Nothing -> return result
  Just workspaceEdit -> case workspaceEditUri workspaceEdit of
    Nothing -> return result
    Just u -> whenReverseLookupUri' u result $ \ds ->
      case untransformWorkspaceEdit ds workspaceEdit of
        Just workspaceEdit' -> return $ set edit (Just workspaceEdit') result
        Nothing -> do
          logWarnN [i|Dropping the edit on resolved code action "#{result ^. title}": it doesn't map back to the notebook|]
          return $ set edit Nothing result

-- | A workspace query names no document, so a hit is put back by the shadow uri it carries
-- rather than one the request told us. An answer can span several shadow documents, so each hit
-- is looked up on its own.
transformServerRsp' config SMethod_WorkspaceSymbol _initialParams result = case result of
  InR (InR LSP.Null) -> return $ InR $ InR LSP.Null
  InL symbolInformations -> (InL . catMaybes) <$> mapM untransform (keep symbolInformations)
  InR (InL workspaceSymbols) -> (InR . InL . catMaybes) <$> mapM untransformWorkspaceSymbol (keep workspaceSymbols)
  where
    -- The wrapper's own symbols are no more worth listing here than in a document's outline
    keep :: HasName a Text => [a] -> [a]
    keep = filter (not . notebookConfigIgnoreSymbol config . (^. name))

    untransform si = whenReverseLookupUri' (si ^. location . uri) (Just si) $ \ds ->
      return $ (\loc -> set location loc si) <$> untransformLocation ds (si ^. location)

    -- A workspace symbol gives a location, or just the file it lives in, which has no range
    untransformWorkspaceSymbol ws = case ws ^. location of
      InL loc -> whenReverseLookupUri' (loc ^. uri) (Just ws) $ \ds ->
        return $ (\loc' -> set location (InL loc') ws) <$> untransformLocation ds loc
      InR uriOnly -> whenReverseLookupUri' (uriOnly ^. uri) (Just ws) $ \(DocumentState {origUri}) ->
        return $ Just (set (location . _R . uri) origUri ws)

transformServerRsp' _ SMethod_TextDocumentCodeLens _initialParams (InR null) = return $ InR null
transformServerRsp' config SMethod_TextDocumentCodeLens initialParams (InL result) = (InL <$>) $
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx, origUri}) -> do
    return $ fmap (stampLensData origUri) (mapMaybe (untransformRanged tx) result)

transformServerRsp' config SMethod_CodeLensResolve initialParams result = case unstampLensData initialParams of
  Nothing -> return result
  Just (u, _) -> whenHandled' (notebookConfigHandles config) u result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return $ fromMaybe result (untransformRanged tx result)

transformServerRsp' _ _ _ result = return result

-- | A prepare-rename answer is the name's range, that range with a placeholder, or a bare
-- "go ahead" with nothing to move.
untransformPrepareRename :: Transformer tx => tx -> (PrepareRenameResult |? Null) -> (PrepareRenameResult |? Null)
untransformPrepareRename _ x@(InR _) = x
untransformPrepareRename tx (InL (PrepareRenameResult answer)) = case answer of
  InL r -> wrap (InL <$> untransformRange tx r)
  InR (InL withPlaceholder) -> wrap (InR . InL <$> untransformRanged tx withPlaceholder)
  InR (InR defaultBehavior) -> InL (PrepareRenameResult (InR (InR defaultBehavior)))
  where
    wrap (Just moved) = InL (PrepareRenameResult moved)
    wrap Nothing = InR LSP.Null

-- | Both Definition and Declaration results come as a location, a list of them, a list of links,
-- or null. A location that doesn't map back is dropped rather than pointing at nothing.
untransformDefinitionResult :: Transformer tx => DocumentState tx -> (Definition |? ([DefinitionLink] |? Null)) -> (Definition |? ([DefinitionLink] |? Null))
untransformDefinitionResult ds (InL (Definition (InL loc))) = case untransformLocation ds loc of
  Just loc' -> InL (Definition (InL loc'))
  Nothing -> InR (InR LSP.Null)
untransformDefinitionResult ds (InL (Definition (InR locs))) = InL (Definition (InR (mapMaybe (untransformLocation ds) locs)))
untransformDefinitionResult ds (InR (InL links)) = InR (InL (mapMaybe (\(DefinitionLink l) -> DefinitionLink <$> untransformLocationLink ds l) links))
untransformDefinitionResult _ x@(InR (InR _)) = x

untransformDeclarationResult :: Transformer tx => DocumentState tx -> (Declaration |? ([DeclarationLink] |? Null)) -> (Declaration |? ([DeclarationLink] |? Null))
untransformDeclarationResult ds (InL (Declaration (InL loc))) = case untransformLocation ds loc of
  Just loc' -> InL (Declaration (InL loc'))
  Nothing -> InR (InR LSP.Null)
untransformDeclarationResult ds (InL (Declaration (InR locs))) = InL (Declaration (InR (mapMaybe (untransformLocation ds) locs)))
untransformDeclarationResult ds (InR (InL links)) = InR (InL (mapMaybe (\(DeclarationLink l) -> DeclarationLink <$> untransformLocationLink ds l) links))
untransformDeclarationResult _ x@(InR (InR _)) = x

-- | A token whose position doesn't map back is dropped; the rest keep their length, type and
-- modifiers.
untransformSemanticTokenData :: Transformer tx => tx -> [UInt] -> [UInt]
untransformSemanticTokenData tx = encodeTokens . sort . mapMaybe move . decodeTokens
  where
    move (line, startChar, rest) = case untransformPosition tx (Position line startChar) of
      Just (Position line' startChar') -> Just (line', startChar', rest)
      Nothing -> Nothing

    decodeTokens = go 0 0
      where
        go prevLine prevStart (deltaLine:deltaStart:len:tokenType:modifiers:more) =
          (line, start, (len, tokenType, modifiers)) : go line start more
          where
            line = prevLine + deltaLine
            start = if deltaLine == 0 then prevStart + deltaStart else deltaStart
        go _ _ _ = []

    encodeTokens = go 0 0
      where
        go _ _ [] = []
        go prevLine prevStart ((line, start, (len, tokenType, modifiers)):more) =
          deltaLine : deltaStart : len : tokenType : modifiers : go line start more
          where
            deltaLine = line - prevLine
            deltaStart = if deltaLine == 0 then start - prevStart else start

-- | A code action whose edit can't be mapped back to the notebook is dropped rather than
-- offered to the client, which would then fail to apply it. Commands pass through: their
-- arguments are server-defined and we have nothing to map.
untransformCodeActionOrCommand :: Transformer tx => DocumentState tx -> (Command |? CodeAction) -> Maybe (Command |? CodeAction)
untransformCodeActionOrCommand _ command@(InL _) = Just command
untransformCodeActionOrCommand ds (InR codeAction) = InR <$> case codeAction ^. edit of
  Nothing -> Just codeAction
  Just workspaceEdit -> flip (set edit) codeAction . Just <$> untransformWorkspaceEdit ds workspaceEdit
