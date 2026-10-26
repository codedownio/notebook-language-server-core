{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Test.DidOpen (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ClientNot
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import System.FilePath
import Test.Sandwich
import TestLib.Config
import TestLib.Folder
import TestLib.Transformers
import UnliftIO.Directory


tests :: TopSpec
tests = describe "textDocument/didOpen" $ do
  it "writes the shadow document" $ do
    dir <- testFolder
    let shadowPath = dir </> "main.ipynb.hs"
    _ <- runDidOpen (shadowConfig shadowPath) (filePathToUri (dir </> "main.ipynb")) "main = x"

    liftIO (T.readFile shadowPath) >>= (`shouldBe` "\nmain = x")

  it "leaves a document the projection didn't shadow alone" $ do
    dir <- testFolder
    let path = dir </> "main.hs"
    liftIO $ T.writeFile path "on disk"
    _ <- runDidOpen (identityConfig path) (filePathToUri path) "in the editor"

    liftIO (T.readFile path) >>= (`shouldBe` "on disk")

  it "doesn't write when the proxy doesn't keep the shadow document saved" $ do
    -- didClose doesn't remove it then either.
    dir <- testFolder
    let shadowPath = dir </> "main.ipynb.hs"
    _ <- runDidOpen ((shadowConfig shadowPath) { notebookConfigDidSaveDebounceMs = Nothing })
                    (filePathToUri (dir </> "main.ipynb")) "main = x"

    doesFileExist shadowPath >>= (`shouldBe` False)

shadowConfig :: FilePath -> NotebookConfig LineShift
shadowConfig shadowPath = testConfig {
  notebookConfigHandles = const True
  , notebookConfigDidSaveDebounceMs = Just 1000
  , notebookConfigMakeShadowDocument = \_ _ ->
      return (filePathToUri shadowPath, shadowPath, mkDocRegex (T.pack shadowPath))
  }

identityConfig :: FilePath -> NotebookConfig LineShift
identityConfig path = (shadowConfig path) {
  notebookConfigTransformerParamsFor = const 0
  }

runDidOpen :: (MonadIO m)
  => NotebookConfig LineShift -> Uri -> Text -> m (MessageParams Method_TextDocumentDidOpen)
runDidOpen config u t = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformClientNot' config (const (return ())) SMethod_TextDocumentDidOpen params) state
  where params = DidOpenTextDocumentParams (TextDocumentItem u (LanguageKind_Custom "test") 1 t)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
