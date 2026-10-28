{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Test.DocumentSymbol (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


-- | An outline arrives in the projection's shape: the cell's symbols hang off whatever
-- wrapper the language needed, and the wrapper spans lines no cell owns.
tests :: TopSpec
tests = describe "textDocument/documentSymbol" $ do
  it "puts a symbol's own range back" $ do
    transformed <- run (InR (InL [symbol "alpha" 8 []]))
    transformed `shouldBe` InR (InL [symbol "alpha" 7 []])

  it "puts a nested symbol's range back too" $ do
    transformed <- run (InR (InL [symbol "Wrapper" 8 [symbol "alpha" 9 []]]))
    transformed `shouldBe` InR (InL [symbol "Wrapper" 7 [symbol "alpha" 8 []]])

  -- The wrapper jnls and gnls add starts on a synthetic line, so it can't be placed in the
  -- notebook. Dropping it outright would take the cell's symbols with it.
  it "replaces a wrapper it can't place with its children" $ do
    transformed <- run (InR (InL [symbol "__Cells" 0 [symbol "alpha" 8 []]]))
    transformed `shouldBe` InR (InL [symbol "alpha" 7 []])

  it "promotes grandchildren through two unplaceable levels" $ do
    transformed <- run (InR (InL [symbol "__Cells" 0 [symbol "__notebookExec" 0 [symbol "alpha" 8 []]]]))
    transformed `shouldBe` InR (InL [symbol "alpha" 7 []])

  it "drops a symbol with nothing under it that it can place" $ do
    transformed <- run (InR (InL [symbol "__Cells" 0 [symbol "synthetic" 0 []]]))
    transformed `shouldBe` InR (InL [])

  -- Ignoring is about the symbol and what it contains, so children go with it.
  it "drops an ignored symbol along with its children" $ do
    let answered = InR (InL [symbol "imports" 8 [symbol "alpha" 9 []]])
    transformed <- runWith (testConfig { notebookConfigIgnoreSymbol = (== "imports") }) answered
    transformed `shouldBe` InR (InL [])

  it "leaves a flat SymbolInformation answer working" $ do
    transformed <- run (InL [symbolInformation "alpha" 8])
    transformed `shouldBe` InL [symbolInformation "alpha" 7]

  it "leaves null alone" $
    run (InR (InR Null)) >>= (`shouldBe` InR (InR Null))

run :: MonadIO m => MessageResult Method_TextDocumentDocumentSymbol -> m (MessageResult Method_TextDocumentDocumentSymbol)
run = runWith testConfig

runWith :: MonadIO m => NotebookConfig LineShift -> MessageResult Method_TextDocumentDocumentSymbol -> m (MessageResult Method_TextDocumentDocumentSymbol)
runWith config result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' config SMethod_TextDocumentDocumentSymbol params result) state
  where params = DocumentSymbolParams Nothing Nothing (TextDocumentIdentifier notebookUri)

symbol :: Text -> UInt -> [DocumentSymbol] -> DocumentSymbol
symbol symbolName line kids = DocumentSymbol symbolName Nothing SymbolKind_Function Nothing Nothing
  (lineRange line) (lineRange line) (if null kids then Nothing else Just kids)

symbolInformation :: Text -> UInt -> SymbolInformation
symbolInformation symbolName line =
  SymbolInformation symbolName SymbolKind_Function Nothing Nothing Nothing
    (Location notebookUri (lineRange line))

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 5)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
