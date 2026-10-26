{-# LANGUAGE OverloadedStrings #-}

-- | A 'NotebookConfig' for the transforms that take one. Only the fields a response transform
-- reads are meaningful; the rest are there to fill the record.
module TestLib.Config (
  testConfig
  ) where

import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import TestLib.Transformers


testConfig :: NotebookConfig LineShift
testConfig = NotebookConfig {
  notebookConfigHandles = \u -> u == notebookUri
  , notebookConfigDidSaveDebounceMs = Nothing
  , notebookConfigHandlesFormatting = True
  , notebookConfigTransformerParamsFor = const 1
  , notebookConfigMakeShadowDocument = \_ _ -> return (shadowUri, "/home/main.ipynb.hs", mkDocRegex "/home/main.ipynb.hs")
  , notebookConfigUnopenedUri = return
  , notebookConfigOnDocumentsChanged = return ()
  , notebookConfigTransformInitialize = return
  , notebookConfigCustomUriMethods = []
  , notebookConfigIgnoreSymbol = const False
  , notebookConfigLanguageName = "test"
  }
