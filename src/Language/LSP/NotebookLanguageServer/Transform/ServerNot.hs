{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.NotebookLanguageServer.Transform.ServerNot where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Transformer


transformServerNot :: (TransformerMonad tx n, Transformer tx, HasJSON (TNotificationMessage m)) => SMethod m -> TNotificationMessage m -> n (TNotificationMessage m)
transformServerNot meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformServerNot' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming server not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'


transformServerNot' :: (TransformerMonad tx n, Transformer tx) => SMethod m -> MessageParams m -> n (MessageParams m)

transformServerNot' SMethod_TextDocumentPublishDiagnostics params = whenReverseLookupUri params $ \(DocumentState {transformer=tx, ..}) -> do
  return $ params
         & set uri origUri
         & over diagnostics (mapMaybe (traverseOf range (untransformRange tx)))

transformServerNot' meth msg = do
  warnNoTransform "server notification" [i|#{meth}|]
  pure msg
