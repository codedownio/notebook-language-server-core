{-# LANGUAGE RankNTypes #-}

module Language.LSP.NotebookLanguageServer.Config (
  NotebookConfig(..)
  ) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Language.LSP.NotebookLanguageServer.Transform.Util
import Text.Regex.PCRE.Light (Regex)


data NotebookConfig tx = NotebookConfig {
  notebookConfigHandles :: Uri -> Bool
  , notebookConfigDidSaveDebounceMs :: Maybe Int
  -- | Whether to rewrite textDocument/formatting at all. Mapping a formatter's output back to
  -- the notebook needs 'unproject', so a language whose transformers don't implement it has to
  -- leave formatting alone: the request keeps the notebook's uri and the response is passed
  -- through untouched, which is what the server would have seen with no proxy in the way.
  , notebookConfigHandlesFormatting :: Bool
  , notebookConfigTransformerParamsFor :: Uri -> Params tx
  , notebookConfigMakeShadowDocument :: forall n. TransformerMonad tx n => Uri -> Doc -> n (Uri, FilePath, Regex)
  , notebookConfigUnopenedUri :: forall n. TransformerMonad tx n => Uri -> n Uri
  , notebookConfigOnDocumentsChanged :: forall n. TransformerMonad tx n => n ()
  , notebookConfigTransformInitialize :: forall n. TransformerMonad tx n => InitializeParams -> n InitializeParams
  , notebookConfigCustomUriMethods :: [Text]
  , notebookConfigIgnoreSymbol :: Text -> Bool
  , notebookConfigLanguageName :: Text
  }
