{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Language.LSP.NotebookLanguageServer.Transform.ClientNot.DidSave where

import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import UnliftIO.Directory


doDidSave :: (TransformerMonad tx m, Transformer tx) => DocumentState tx -> SendExtraNotificationFn m -> m ()
doDidSave ds@(DocumentState {..}) sendExtraNotification = do
  whenServerCapabilitiesSatisfy supportsWillSave $ \_ ->
    sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentWillSave $ WillSaveTextDocumentParams {
      _textDocument = TextDocumentIdentifier newUri
      , _reason = TextDocumentSaveReason_AfterDelay
      }

  writeShadowDocument ds

  whenServerCapabilitiesSatisfy supportsSave $ \maybeSaveOptions ->
    sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentDidSave $ DidSaveTextDocumentParams {
      _textDocument = TextDocumentIdentifier newUri
      , _text = case maybeSaveOptions of
          Just (SaveOptions {_includeText=(Just True)}) -> Just (Rope.toText curLines')
          _ -> Nothing
      }

-- | Put the projected document on disk, where a server that works from files can read it.
writeShadowDocument :: MonadIO m => DocumentState tx -> m ()
writeShadowDocument (DocumentState {..})
  | newUri == origUri = pure ()
  | otherwise = liftIO $ T.writeFile newPath (Rope.toText curLines')

removeShadowDocument :: MonadIO m => DocumentState tx -> m ()
removeShadowDocument (DocumentState {..})
  | newUri == origUri = pure ()
  | otherwise = removePathForcibly newPath
