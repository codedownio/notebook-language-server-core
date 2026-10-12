{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.NotebookLanguageServer.Transform.ClientNot where

import Control.Debounce
import Control.Lens hiding ((:>), (<.>), List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Data.Time
import qualified Data.UUID.V4 as UUID
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.FilePath
import Language.LSP.NotebookLanguageServer.Transform.ClientNot.DidSave
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import UnliftIO.Concurrent
import UnliftIO.Directory
import Language.LSP.NotebookLanguageServer.Config


type ClientNotMethod m = SMethod (m :: Method 'ClientToServer 'Notification)

transformClientNot :: (
  TransformerMonad tx n, Transformer tx, ProjectIn n tx, HasJSON (TNotificationMessage m)
  ) => NotebookConfig tx
    -> SendExtraNotificationFn n
    -> ClientNotMethod m
    -> TNotificationMessage m
    -> n (TNotificationMessage m)
transformClientNot config sendExtraNotification meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientNot' config sendExtraNotification meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientNot' :: (
  TransformerMonad tx n, Transformer tx, ProjectIn n tx
  ) => NotebookConfig tx
    -> (forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ())
    -> ClientNotMethod m
    -> MessageParams m
    -> n (MessageParams m)

transformClientNot' config sendExtraNotification SMethod_TextDocumentDidOpen params = whenAnything params $ \u -> do
  let t = params ^. (textDocument . text)
  let ls = Rope.fromText t
  let txParams = if isNotebook u then notebookConfigTransformerParams config else notebookConfigIdTransformerParams config
  (ls', transformer', eitherErr) <- project txParams ls
  case eitherErr of
    Left err -> logWarnN [i|(#{u}) Project failed: #{err}|]
    Right () -> return ()
  TransformerState {..} <- ask
  (newUri, newPath, referenceRegex) <- notebookConfigMakeShadowDocument config u ls'

  uuid <- liftIO UUID.nextRandom

  debouncedDidChange <- withRunInIO $ \runInIO -> mkDebounce (
    defaultDebounceSettings {
        debounceAction = do
          withMVar transformerDocuments $ \m ->
            case M.lookup (getUri u) m of
              (Just ds@(DocumentState {..}))
                | documentUuid == uuid -> runInIO $ doDidSave ds sendExtraNotification
                | otherwise -> return ()
              _ -> return ()
        , debounceFreq = transformerDidSaveDebouncePeriodMs * 1000
        , debounceEdge = trailingEdge
        })

  modifyMVar_ transformerDocuments $ \x -> return $! M.insert (getUri u) (
    DocumentState {
        transformer = transformer'
        , curLines = ls
        , curLines' = ls'
        , origUri = u
        , newUri = newUri
        , newPath = newPath
        , referenceRegex = referenceRegex
        , documentUuid = uuid
        , debouncedDidChange = debouncedDidChange
        }) x

  notebookConfigOnDocumentsChanged config

  return $ params
         & set (textDocument . text) (Rope.toText ls')
         & set (textDocument . uri) newUri

transformClientNot' config _ SMethod_TextDocumentDidChange params = whenAnything params $ modifyTransformer params $ \ds@(DocumentState {transformer=tx, curLines=before, curLines'=before', origUri, newUri, debouncedDidChange}) -> do
  let txParams = if isNotebook origUri then notebookConfigTransformerParams config else notebookConfigIdTransformerParams config
  let changeEvents = params ^. contentChanges
  (changeEvents', tx') <- handleDiffMulti txParams before changeEvents tx
  let after = applyChanges changeEvents before
  let after' = applyChanges changeEvents' before'

  liftIO debouncedDidChange

  return (
    ds { transformer = tx'
       , curLines = after
       , curLines' = after'
       }
    , params & set contentChanges changeEvents'
             & set (textDocument . uri) newUri
    )

transformClientNot' config _ SMethod_TextDocumentDidClose params = whenAnything params $ \u -> do
  TransformerState {..} <- ask
  maybeDocumentState <- modifyMVar transformerDocuments (return . flipTuple . M.updateLookupWithKey (\_ _ -> Nothing) (getUri u))
  newUri <- case maybeDocumentState of
    Just (DocumentState {..}) -> do
      removePathForcibly newPath
      pure newUri
    Nothing -> notebookConfigUnopenedUri config u
  notebookConfigOnDocumentsChanged config
  return $ params
         & set (textDocument . uri) newUri

transformClientNot' config _ _ params = return params
