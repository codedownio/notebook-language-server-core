{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hierarchy (tests, main) where

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
tests = describe "Hierarchies" $ do
  describe "textDocument/prepareCallHierarchy" $ do
    it "puts the prepared item back on the notebook" $ do
      transformed <- runRsp SMethod_TextDocumentPrepareCallHierarchy (positionParams 7) (InL [callItem shadowUri 8])
      transformed `shouldBe` InL [callItem notebookUri 7]

    it "leaves an item in a document we aren't shadowing alone" $ do
      let elsewhere = InL [callItem (Uri "file:///home/other.rs") 8]
      runRsp SMethod_TextDocumentPrepareCallHierarchy (positionParams 7) elsewhere >>= (`shouldBe` elsewhere)

    it "drops an item that doesn't map back into a cell" $ do
      transformed <- runRsp SMethod_TextDocumentPrepareCallHierarchy (positionParams 7)
                       (InL [callItem shadowUri 0, callItem shadowUri 8])
      transformed `shouldBe` InL [callItem notebookUri 7]

  describe "callHierarchy/incomingCalls" $ do
    it "sends the item the client was given back in the shadow document's terms" $ do
      transformed <- runReq SMethod_CallHierarchyIncomingCalls (CallHierarchyIncomingCallsParams Nothing Nothing (callItem notebookUri 7))
      transformed `shouldBe` CallHierarchyIncomingCallsParams Nothing Nothing (callItem shadowUri 8)

    it "puts the caller and the ranges the calls are on back" $ do
      let answered = InL [CallHierarchyIncomingCall (callItem shadowUri 8) [lineRange 9]]
      transformed <- runRsp SMethod_CallHierarchyIncomingCalls (incomingParams 7) answered
      transformed `shouldBe` InL [CallHierarchyIncomingCall (callItem notebookUri 7) [lineRange 8]]

    it "leaves a caller in another document alone, ranges and all" $ do
      let elsewhere = InL [CallHierarchyIncomingCall (callItem (Uri "file:///home/other.rs") 8) [lineRange 9]]
      runRsp SMethod_CallHierarchyIncomingCalls (incomingParams 7) elsewhere >>= (`shouldBe` elsewhere)

  describe "callHierarchy/outgoingCalls" $
    it "puts the callee and the ranges the calls are written on back" $ do
      let answered = InL [CallHierarchyOutgoingCall (callItem shadowUri 12) [lineRange 9]]
      transformed <- runRsp SMethod_CallHierarchyOutgoingCalls (outgoingParams 7) answered
      transformed `shouldBe` InL [CallHierarchyOutgoingCall (callItem notebookUri 11) [lineRange 8]]

  describe "the type hierarchy" $ do
    it "puts a prepared type back on the notebook" $ do
      transformed <- runRsp SMethod_TextDocumentPrepareTypeHierarchy (typePositionParams 7) (InL [typeItem shadowUri 8])
      transformed `shouldBe` InL [typeItem notebookUri 7]

    it "puts a supertype back" $ do
      transformed <- runRsp SMethod_TypeHierarchySupertypes (supertypesParams 7) (InL [typeItem shadowUri 8])
      transformed `shouldBe` InL [typeItem notebookUri 7]

    it "sends a subtype query's item back in the shadow document's terms" $ do
      transformed <- runReq SMethod_TypeHierarchySubtypes (TypeHierarchySubtypesParams Nothing Nothing (typeItem notebookUri 7))
      transformed `shouldBe` TypeHierarchySubtypesParams Nothing Nothing (typeItem shadowUri 8)

runRsp :: (MonadIO m, Show (MessageResult meth))
  => SMethod (meth :: Method 'ClientToServer 'Request) -> MessageParams meth -> MessageResult meth -> m (MessageResult meth)
runRsp method params result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' testConfig method params result) state

runReq :: (MonadIO m)
  => SMethod (meth :: Method 'ClientToServer 'Request) -> MessageParams meth -> m (MessageParams meth)
runReq method params = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig method params) state

positionParams :: UInt -> CallHierarchyPrepareParams
positionParams line = CallHierarchyPrepareParams (TextDocumentIdentifier notebookUri) (Position line 0) Nothing

typePositionParams :: UInt -> TypeHierarchyPrepareParams
typePositionParams line = TypeHierarchyPrepareParams (TextDocumentIdentifier notebookUri) (Position line 0) Nothing

incomingParams :: UInt -> CallHierarchyIncomingCallsParams
incomingParams line = CallHierarchyIncomingCallsParams Nothing Nothing (callItem notebookUri line)

outgoingParams :: UInt -> CallHierarchyOutgoingCallsParams
outgoingParams line = CallHierarchyOutgoingCallsParams Nothing Nothing (callItem notebookUri line)

supertypesParams :: UInt -> TypeHierarchySupertypesParams
supertypesParams line = TypeHierarchySupertypesParams Nothing Nothing (typeItem notebookUri line)

callItem :: Uri -> UInt -> CallHierarchyItem
callItem u line = CallHierarchyItem "f" SymbolKind_Function Nothing Nothing u (lineRange line) (lineRange line) Nothing

typeItem :: Uri -> UInt -> TypeHierarchyItem
typeItem u line = TypeHierarchyItem "T" SymbolKind_Class Nothing Nothing u (lineRange line) (lineRange line) Nothing

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 5)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
