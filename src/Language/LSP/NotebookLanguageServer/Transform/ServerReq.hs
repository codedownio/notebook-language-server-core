{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Language.LSP.NotebookLanguageServer.Transform.ServerReq (
  transformServerReq
  ) where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Transformer


transformServerReq :: (
  TransformerMonad tx n, Transformer tx, HasJSON (TRequestMessage m)
  ) => SMethod m -> TRequestMessage m -> n (TRequestMessage m)
transformServerReq meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformServerReq' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming server req #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformServerReq' :: (TransformerMonad tx n, Transformer tx) => SMethod m -> MessageParams m -> n (MessageParams m)

-- | A server asking the client to apply an edit names the shadow document, at the shadow
-- document's coordinates. The client only ever knows the notebook: it has no idea the shadow
-- file exists, and no way to know what the transformers did to the lines, so this is the only
-- place either can be undone.
transformServerReq' SMethod_WorkspaceApplyEdit params = case workspaceEditUri (params ^. edit) of
  Nothing -> return params
  Just u -> whenReverseLookupUri' u params $ \ds ->
    case untransformWorkspaceEdit ds (params ^. edit) of
      Just edit' -> return $ set edit edit' params
      -- Passing the edit through unchanged leaves the client to refuse it, naming a document
      -- it doesn't have open. That's no worse than dropping it, and the log says why.
      Nothing -> do
        logWarnN [i|Couldn't map a workspace/applyEdit back to the notebook: #{A.encode (params ^. edit)}|]
        return params

transformServerReq' meth params = do
  warnNoTransform "server request" [i|#{meth}|]
  pure params
