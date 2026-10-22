{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.SemanticTokens (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


-- Three tokens: two on shadow line 4, one on shadow line 7, delta encoded
shadowTokens :: [UInt]
shadowTokens = [4, 2, 3, 1, 0, 0, 6, 5, 2, 1, 3, 0, 7, 3, 0]

tests :: TopSpec
tests = describe "SemanticTokens" $ do
  describe "textDocument/semanticTokens/full" $ do
    it "moves every token up by the lines the shadow document added" $ do
      transformed <- runFull (InL (SemanticTokens Nothing shadowTokens))
      transformed `shouldBe` InL (SemanticTokens Nothing [3, 2, 3, 1, 0, 0, 6, 5, 2, 1, 3, 0, 7, 3, 0])

    it "keeps the result id" $ do
      transformed <- runFull (InL (SemanticTokens (Just "17") shadowTokens))
      transformed `shouldBe` InL (SemanticTokens (Just "17") [3, 2, 3, 1, 0, 0, 6, 5, 2, 1, 3, 0, 7, 3, 0])

    it "drops a token on a line that isn't a cell's" $ do
      -- The first token sits on shadow line 0, which is the header the transformer added
      transformed <- runFull (InL (SemanticTokens Nothing [0, 0, 2, 9, 0, 4, 2, 3, 1, 0]))
      transformed `shouldBe` InL (SemanticTokens Nothing [3, 2, 3, 1, 0])

    it "leaves an empty answer alone" $
      runFull (InL (SemanticTokens Nothing [])) >>= (`shouldBe` InL (SemanticTokens Nothing []))

    it "leaves null alone" $
      runFull (InR Null) >>= (`shouldBe` InR Null)

runFull :: MonadIO m
  => MessageResult Method_TextDocumentSemanticTokensFull -> m (MessageResult Method_TextDocumentSemanticTokensFull)
runFull result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformServerRsp' testConfig SMethod_TextDocumentSemanticTokensFull params result) state
  where params = SemanticTokensParams Nothing Nothing (TextDocumentIdentifier notebookUri)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
