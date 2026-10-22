{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.NotebookLanguageServer.Transform.Util where

import Control.Lens hiding ((:>), (<.>))
import Control.Lens.Regex.Text
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Transformer
import Network.URI
import System.FilePath
import qualified System.Random as R
import UnliftIO.MVar


type SendExtraNotificationFn n = forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ()

-- * whenHandled

whenHandled :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => (Uri -> Bool) -> a -> (Uri -> n a) -> n a
whenHandled handles params = whenHandled' handles (params ^. (textDocument . uri)) params

whenHandledByInitialParams :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => (Uri -> Bool) -> a -> c -> (Uri -> n c) -> n c
whenHandledByInitialParams handles params = whenHandled' handles (params ^. (textDocument . uri))

whenHandled' :: (MonadLoggerIO n) => (Uri -> Bool) -> Uri -> a -> (Uri -> n a) -> n a
whenHandled' handles uri params action = if
  | handles uri -> action uri
  | otherwise -> pure params

isNotebook :: Uri -> Bool
isNotebook uri = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> False
  Just (URI {..}) -> ".ipynb" `L.isSuffixOf` fmap C.toLower uriPath

-- * whenNotebookResult

whenReverseLookupUri :: (TransformerMonad tx n, Transformer tx, MonadLoggerIO n, HasUri a Uri) => a -> (DocumentState tx -> n a) -> n a
whenReverseLookupUri params = whenReverseLookupUri' (params ^. uri) params

-- | Note that this takes in server URIs and calls the callback with original URIs
-- TODO: make this more efficient
whenReverseLookupUri' :: (TransformerMonad tx n, Transformer tx) => Uri -> a -> (DocumentState tx -> n a) -> n a
whenReverseLookupUri' uri params cb = do
  txDocumentsVar <- asks transformerDocuments
  txDocuments <- readMVar txDocumentsVar
  flip fix (M.toList txDocuments) $ \loop items -> case items of
    [] -> return params
    ((_, ds@(DocumentState {..})):rest) ->
      if | newUri == uri -> cb ds
         | otherwise -> loop rest

-- * TransformerMonad

type TransformerMonad tx n = (
  MonadLoggerIO n
  , MonadReader (TransformerState tx) n
  , MonadUnliftIO n
  , MonadFail n
  )

-- * TransformerState

data DocumentState tx = DocumentState {
  transformer :: tx
  , curLines :: Doc
  , curLines' :: Doc
  , origUri :: Uri
  , newUri :: Uri
  , newPath :: FilePath
  , referenceRegex :: Regex
  , documentUuid :: UUID
  , debouncedDidChange :: IO ()
  }

data TransformerState tx = TransformerState {
  transformerDocuments :: MVar (M.Map Text (DocumentState tx))
  , transformerInitializeParams :: MVar (Maybe InitializeParams)
  , transformerInitializeResult :: MVar (Maybe InitializeResult)
  -- | Methods 'warnNoTransform' has already spoken up about
  , transformerUnhandled :: MVar (S.Set Text)
  }

-- * Catching a missing transform

-- | Reaching a transform table's catch-all means no arm claimed the method, so whatever it
-- names goes past untranslated: the wrapped server is handed the notebook's path, or the client
-- the shadow document's. Either way it surfaces at the far end, as a confusing @file not
-- found@, rather than here where the arm is missing.
--
-- Plenty of methods belong in the catch-all -- they name no document and have nothing to
-- translate -- so this says it once per method and then keeps quiet, which makes the log worth
-- reading without having to list every method that is fine.
warnNoTransform :: TransformerMonad tx n => Text -> Text -> n ()
warnNoTransform kind method = do
  TransformerState {transformerUnhandled} <- ask
  modifyMVar_ transformerUnhandled $ \seen -> case S.member key seen of
    True -> return seen
    False -> do
      logWarnN [i|No transform for #{kind} #{method}. If it names a document, the far side will be given one it doesn't know about.|]
      return (S.insert key seen)
  where key = kind <> " " <> method

-- * Transformers

newTransformerState :: (MonadIO m) => m (TransformerState tx)
newTransformerState = TransformerState
  <$> newMVar mempty
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> newMVar mempty

lookupTransformer :: (TransformerMonad tx m, Transformer tx) => Uri -> m (Maybe (DocumentState tx))
lookupTransformer uri = do
  TransformerState {..} <- ask
  M.lookup (getUri uri) <$> readMVar transformerDocuments

withTransformer :: (TransformerMonad tx n, Transformer tx) => a -> (DocumentState tx -> n a) -> Uri -> n a
withTransformer def cb uri = do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return def
    Just tx -> cb tx

modifyTransformer :: (TransformerMonad tx n, Transformer tx) => a -> (DocumentState tx -> n (DocumentState tx, a)) -> Uri -> n a
modifyTransformer def cb uri = do
  TransformerState {..} <- ask
  modifyMVar transformerDocuments $ \m -> case M.lookup (getUri uri) m of
    Nothing -> return (m, def)
    Just tx -> do
      (tx', ret) <- cb tx
      return (M.insert (getUri uri) tx' m, ret)

-- * Checking server capabilities

whenServerCapabilitiesSatisfy :: (TransformerMonad tx n, Transformer tx) => (ServerCapabilities -> (Bool, a)) -> (a -> n ()) -> n ()
whenServerCapabilitiesSatisfy cb action = do
  initializeResultVar <- asks transformerInitializeResult
  readMVar initializeResultVar >>= \case
    Just ((cb . (^. capabilities)) -> (True, sideValue)) -> action sideValue
    _ -> return ()

supportsWillSave :: ServerCapabilities -> (Bool, ())
supportsWillSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_willSave=(Just True)}))) }) = (True, ())
supportsWillSave _ = (False, ())

supportsSave :: ServerCapabilities -> (Bool, Maybe SaveOptions)
supportsSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_save}))) }) = case _save of
  Nothing -> (False, Nothing)
  Just (InL x) -> (x, Nothing)
  Just (InR saveOptions) -> (True, Just saveOptions)
supportsSave _ = (False, Nothing)

-- * Random identifiers

-- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
-- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['A'..'Z']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID' :: MonadIO m => Int -> m String
makeUUID' n = replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1))

-- * Misc

addExtensionToUri :: (MonadLogger m) => String -> Uri -> m Uri
addExtensionToUri ext u@(Uri t) = case parseURIReference (T.unpack t) of
  Nothing -> do
    logErrorN [i|Couldn't parse URI: #{t}|]
    return u
  Just uri -> return $ Uri $ T.pack $ show $ uri { uriPath = uriPath uri <.> ext }

flipTuple :: (b, a) -> (a, b)
flipTuple (x, y) = (y, x)
