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
  , notebookConfigWriteFileOnChange :: Bool
  , notebookConfigTransformerParams :: Params tx
  , notebookConfigIdTransformerParams :: Params tx
  , notebookConfigMakeShadowDocument :: forall n. TransformerMonad tx n => Uri -> Doc -> n (Uri, FilePath, Regex)
  , notebookConfigUnopenedUri :: forall n. TransformerMonad tx n => Uri -> n Uri
  , notebookConfigOnDocumentsChanged :: forall n. TransformerMonad tx n => n ()
  , notebookConfigTransformInitialize :: forall n. TransformerMonad tx n => InitializeParams -> n InitializeParams
  , notebookConfigCustomUriMethods :: [Text]
  , notebookConfigIgnoreSymbol :: Text -> Bool
  , notebookConfigLanguageName :: Text
  }
