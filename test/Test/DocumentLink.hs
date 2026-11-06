{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.DocumentLink (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.NotebookLanguageServer.Transform.ClientReq
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "DocumentLink" $ do
  describe "the request" $ do
    it "names the shadow document" $ do
      transformed <- runRequest (params notebookUri)
      transformed `shouldBe` params shadowUri

    it "leaves a document we aren't shadowing alone" $ do
      let elsewhere = params (Uri "file:///home/other.rs")
      runRequest elsewhere >>= (`shouldBe` elsewhere)

  describe "the response" $ do
    it "moves each link back to the notebook and stamps it with the notebook" $ do
      transformed <- runResponse (InL [link 4, link 6])
      transformed `shouldBe` InL [stampNotebookData notebookUri (link 3), stampNotebookData notebookUri (link 5)]

    it "drops a link that isn't in a cell" $ do
      transformed <- runResponse (InL [link 0])
      transformed `shouldBe` InL []

    it "leaves null alone" $
      runResponse (InR Null) >>= (`shouldBe` InR Null)

  describe "resolving" $ do
    it "moves the stamped link into the shadow document and back" $ do
      sent <- runResolveRequest (stampNotebookData notebookUri (link 3))
      sent `shouldBe` link 4

      resolved <- runResolveResponse (stampNotebookData notebookUri (link 3)) (link 4)
      resolved `shouldBe` link 3

runRequest :: MonadIO m => DocumentLinkParams -> m DocumentLinkParams
runRequest p = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig SMethod_TextDocumentDocumentLink p) state

runResponse :: MonadIO m
  => MessageResult Method_TextDocumentDocumentLink -> m (MessageResult Method_TextDocumentDocumentLink)
runResponse result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformServerRsp' testConfig SMethod_TextDocumentDocumentLink (params notebookUri) result) state

runResolveRequest :: MonadIO m => DocumentLink -> m DocumentLink
runResolveRequest l = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig SMethod_DocumentLinkResolve l) state

runResolveResponse :: MonadIO m => DocumentLink -> DocumentLink -> m DocumentLink
runResolveResponse sent result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' testConfig SMethod_DocumentLinkResolve sent result) state

params :: Uri -> DocumentLinkParams
params u = DocumentLinkParams Nothing Nothing (TextDocumentIdentifier u)

link :: UInt -> DocumentLink
link l = DocumentLink (Range (Position l 0) (Position l 5)) (Just (Uri "https://example.com")) Nothing Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
