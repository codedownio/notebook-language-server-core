{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.LSP.NotebookLanguageServer.Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import GHC.TypeLits (symbolVal)
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.Util
import UnliftIO.Concurrent


type ClientReqMethod m = SMethod (m :: Method 'ClientToServer 'Request)


transformClientReq :: (TransformerMonad tx n, Transformer tx, HasJSON (TRequestMessage m)) => NotebookConfig tx -> ClientReqMethod m -> TRequestMessage m -> n (TRequestMessage m)
transformClientReq config meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientReq' config meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client req #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientReq' :: forall m n tx. (TransformerMonad tx n, Transformer tx) => NotebookConfig tx -> ClientReqMethod m -> MessageParams m -> n (MessageParams m)

transformClientReq' config SMethod_Initialize params = do
  -- Store the non-modified params, so we can access the unmodified rootUri
  asks transformerInitializeParams >>= flip modifyMVar_ (\_ -> return $ Just params)

  notebookConfigTransformInitialize config params

transformClientReq' config SMethod_TextDocumentCodeAction params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndRange @m params
transformClientReq' config SMethod_TextDocumentCodeLens params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUri @m params
transformClientReq' config SMethod_TextDocumentCompletion params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentDefinition params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentDocumentHighlight params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentDocumentSymbol params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUri @m params
transformClientReq' config SMethod_TextDocumentFormatting params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUri @m params
transformClientReq' config SMethod_TextDocumentHover params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentImplementation params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentInlayHint params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndRange @m params
transformClientReq' config SMethod_TextDocumentOnTypeFormatting params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' config SMethod_TextDocumentTypeDefinition params = whenHandled (notebookConfigHandles config) params $ withTransformer params $ doTransformUriAndPosition @m params

transformClientReq' config (SMethod_CustomMethod p) val
  | T.pack (symbolVal p) `elem` notebookConfigCustomUriMethods config = case A.fromJSON val of
      A.Error err -> logErrorN [i|Failed to decode custom method "#{symbolVal p}": #{err}|] >> return val
      A.Success (TextDocumentIdentifier uri) ->
        whenHandled' (notebookConfigHandles config) uri val $
          withTransformer val $ \(DocumentState {newUri}) ->
            return $ A.toJSON $ TextDocumentIdentifier newUri
  | otherwise = return val

transformClientReq' _ _ params = return params


doTransformUriAndPosition :: forall m n a tx. (
  TransformerMonad tx n, Transformer tx, HasPosition (MessageParams m) Position, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState tx -> n (MessageParams m)
doTransformUriAndPosition params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformPosition tx (params ^. position) of
    Nothing -> do
      logWarnN [i|Couldn't transform position #{params ^. position}|]
      return params
    Just pos' -> return $ set position pos' params

doTransformUriAndRange :: forall m n a tx. (
  TransformerMonad tx n, Transformer tx, HasRange (MessageParams m) Range, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState tx -> n (MessageParams m)
doTransformUriAndRange params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformRange tx (params ^. range) of
    Nothing -> do
      logWarnN [i|Couldn't transform range #{params ^. range}|]
      return params
    Just pos' -> return $ set range pos' params

doTransformUri :: forall m n a tx. (
  TransformerMonad tx n, Transformer tx, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState tx -> n (MessageParams m)
doTransformUri params' (DocumentState {newUri}) = return $ params' & set (textDocument . uri) newUri
