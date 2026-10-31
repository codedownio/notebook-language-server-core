{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Diagnostics (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import Language.LSP.NotebookLanguageServer.Transform.ClientReq
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "Pulled diagnostics" $ do
  it "asks the shadow document about the notebook the client named" $ do
    transformed <- runReq SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri)
    transformed `shouldBe` diagnosticParams shadowUri

  it "leaves a query about a document we aren't shadowing alone" $ do
    let elsewhere = diagnosticParams (Uri "file:///home/other.hs")
    runReq SMethod_TextDocumentDiagnostic elsewhere >>= (`shouldBe` elsewhere)

  it "puts a reported diagnostic back on the cell's line" $ do
    transformed <- runRsp SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri) (fullReport [diagnosticOn 8] Nothing)
    transformed `shouldBe` fullReport [diagnosticOn 7] Nothing

  it "drops one that doesn't map back into a cell" $ do
    transformed <- runRsp SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri) (fullReport [diagnosticOn 0, diagnosticOn 8] Nothing)
    transformed `shouldBe` fullReport [diagnosticOn 7] Nothing

  it "names the notebook, not the shadow document, in a related report" $ do
    let related = M.singleton shadowUri (InL (FullDocumentDiagnosticReport (AString :: AString "full") Nothing [diagnosticOn 8]))
    transformed <- runRsp SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri) (fullReport [] (Just related))
    transformed `shouldBe` fullReport [] (Just (M.singleton notebookUri (InL (FullDocumentDiagnosticReport (AString :: AString "full") Nothing [diagnosticOn 7]))))

  it "leaves a related report on another document alone" $ do
    let other = Uri "file:///home/other.hs"
    let related = M.singleton other (InL (FullDocumentDiagnosticReport (AString :: AString "full") Nothing [diagnosticOn 8]))
    runRsp SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri) (fullReport [] (Just related))
      >>= (`shouldBe` fullReport [] (Just related))

  it "has nothing to move in an unchanged report" $ do
    let unchanged = DocumentDiagnosticReport (InR (RelatedUnchangedDocumentDiagnosticReport (AString :: AString "unchanged") "abc" Nothing))
    runRsp SMethod_TextDocumentDiagnostic (diagnosticParams notebookUri) unchanged >>= (`shouldBe` unchanged)

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

diagnosticParams :: Uri -> DocumentDiagnosticParams
diagnosticParams u = DocumentDiagnosticParams Nothing Nothing (TextDocumentIdentifier u) Nothing Nothing

fullReport :: [Diagnostic] -> Maybe (M.Map Uri (FullDocumentDiagnosticReport |? UnchangedDocumentDiagnosticReport)) -> DocumentDiagnosticReport
fullReport diags related = DocumentDiagnosticReport (InL (RelatedFullDocumentDiagnosticReport (AString :: AString "full") Nothing diags related))

diagnosticOn :: UInt -> Diagnostic
diagnosticOn l = Diagnostic (Range (Position l 0) (Position l 5)) Nothing Nothing Nothing Nothing "mismatched types" Nothing Nothing Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
