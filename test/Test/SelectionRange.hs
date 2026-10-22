{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.SelectionRange (tests, main) where

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
tests = describe "SelectionRange" $ do
  describe "the request" $ do
    it "moves every position into the shadow document" $ do
      transformed <- runRequest (params notebookUri [Position 3 1, Position 5 2])
      transformed `shouldBe` params shadowUri [Position 4 1, Position 6 2]

    it "leaves a document we aren't shadowing alone" $ do
      let elsewhere = params (Uri "file:///home/other.rs") [Position 3 1]
      runRequest elsewhere >>= (`shouldBe` elsewhere)

  describe "the response" $ do
    it "walks the whole chain back to the notebook" $ do
      let answered = InL [chain [4, 6, 9]]
      transformed <- runResponse answered
      transformed `shouldBe` InL [chain [3, 5, 8]]

    it "drops a chain that doesn't map back into a cell" $ do
      -- The outermost range covers the header line the transformer added
      transformed <- runResponse (InL [chain [4, 0]])
      transformed `shouldBe` InL []

    it "leaves null alone" $
      runResponse (InR Null) >>= (`shouldBe` InR Null)

runRequest :: MonadIO m => SelectionRangeParams -> m SelectionRangeParams
runRequest p = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig SMethod_TextDocumentSelectionRange p) state

runResponse :: MonadIO m
  => MessageResult Method_TextDocumentSelectionRange -> m (MessageResult Method_TextDocumentSelectionRange)
runResponse result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformServerRsp' testConfig SMethod_TextDocumentSelectionRange (params notebookUri [Position 3 1]) result) state

params :: Uri -> [Position] -> SelectionRangeParams
params u ps = SelectionRangeParams Nothing Nothing (TextDocumentIdentifier u) ps

-- Innermost first, each one's parent being the next
chain :: [UInt] -> SelectionRange
chain [] = error "a selection range is at least one range"
chain (l:ls) = SelectionRange (lineRange l) (case ls of { [] -> Nothing; _ -> Just (chain ls) })

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 5)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
