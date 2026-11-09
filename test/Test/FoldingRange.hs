{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.FoldingRange (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.NotebookLanguageServer.Transform.ClientReq
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "FoldingRange" $ do
  describe "the request" $ do
    it "names the shadow document" $ do
      transformed <- runRequest (params notebookUri)
      transformed `shouldBe` params shadowUri

    it "leaves a document we aren't shadowing alone" $ do
      let elsewhere = params (Uri "file:///home/other.rs")
      runRequest elsewhere >>= (`shouldBe` elsewhere)

  describe "the response" $ do
    it "moves each fold back to the notebook" $ do
      transformed <- runResponse (InL [fold 2 4 Nothing, fold 5 8 (Just 3)])
      transformed `shouldBe` InL [fold 1 3 Nothing, fold 4 7 (Just 3)]

    it "drops a fold that starts on a line the cells don't have" $ do
      transformed <- runResponse (InL [fold 0 4 Nothing])
      transformed `shouldBe` InL []

    it "leaves null alone" $
      runResponse (InR Null) >>= (`shouldBe` InR Null)

runRequest :: MonadIO m => FoldingRangeParams -> m FoldingRangeParams
runRequest p = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig SMethod_TextDocumentFoldingRange p) state

runResponse :: MonadIO m
  => MessageResult Method_TextDocumentFoldingRange -> m (MessageResult Method_TextDocumentFoldingRange)
runResponse result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformServerRsp' testConfig SMethod_TextDocumentFoldingRange (params notebookUri) result) state

params :: Uri -> FoldingRangeParams
params u = FoldingRangeParams Nothing Nothing (TextDocumentIdentifier u)

fold :: UInt -> UInt -> Maybe UInt -> FoldingRange
fold s e c = FoldingRange s c e c Nothing Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
