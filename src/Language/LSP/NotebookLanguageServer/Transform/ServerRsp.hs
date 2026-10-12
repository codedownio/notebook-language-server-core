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
      InR (InL documentSymbols) -> return $ InR $ InL $ (documentSymbols & filter (not . ignoreSymbol)
                                                                         & mapMaybe (traverseOf range (untransformRange tx))
                                                                         & mapMaybe (traverseOf selectionRange (untransformRange tx)))
      InR (InR LSP.Null) -> return $ InR $ InR LSP.Null
  where
    ignoreSymbol :: HasName a Text => a -> Bool
    ignoreSymbol sym = notebookConfigIgnoreSymbol config (sym ^. name)

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

transformServerRsp' _ SMethod_TextDocumentCodeLens _initialParams (InR null) = return $ InR null
transformServerRsp' config SMethod_TextDocumentCodeLens initialParams (InL result) = (InL <$>) $
  whenHandledByInitialParams (notebookConfigHandles config) initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    return $ mapMaybe (untransformRanged tx) result

transformServerRsp' _ _ _ result = return result

-- | A code action whose edit can't be mapped back to the notebook is dropped rather than
-- offered to the client, which would then fail to apply it. Commands pass through: their
-- arguments are server-defined and we have nothing to map.
untransformCodeActionOrCommand :: Transformer tx => DocumentState tx -> (Command |? CodeAction) -> Maybe (Command |? CodeAction)
untransformCodeActionOrCommand _ command@(InL _) = Just command
untransformCodeActionOrCommand ds (InR codeAction) = InR <$> case codeAction ^. edit of
  Nothing -> Just codeAction
  Just workspaceEdit -> flip (set edit) codeAction . Just <$> untransformWorkspaceEdit ds workspaceEdit
