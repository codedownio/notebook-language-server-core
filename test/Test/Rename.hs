{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Test.Rename (tests, main) where

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
tests = describe "Rename" $ do
  describe "the requests" $ do
    -- These reached the wrapped server naming the notebook, which answered "file not found"
    it "moves a rename into the shadow document" $ do
      transformed <- liftIO $ runRequest SMethod_TextDocumentRename
        (RenameParams Nothing (TextDocumentIdentifier notebookUri) (Position 3 4) "newName")
      transformed `shouldBe` RenameParams Nothing (TextDocumentIdentifier shadowUri) (Position 4 4) "newName"

    it "moves a prepareRename into the shadow document" $ do
      transformed <- liftIO $ runRequest SMethod_TextDocumentPrepareRename
        (PrepareRenameParams (TextDocumentIdentifier notebookUri) (Position 3 4) Nothing)
      transformed `shouldBe` PrepareRenameParams (TextDocumentIdentifier shadowUri) (Position 4 4) Nothing

    it "moves references, declaration and signatureHelp too" $ do
      refs <- liftIO $ runRequest SMethod_TextDocumentReferences
        (ReferenceParams (TextDocumentIdentifier notebookUri) (Position 3 4) Nothing Nothing (ReferenceContext True))
      refs `shouldBe` ReferenceParams (TextDocumentIdentifier shadowUri) (Position 4 4) Nothing Nothing (ReferenceContext True)

      decl <- liftIO $ runRequest SMethod_TextDocumentDeclaration
        (DeclarationParams (TextDocumentIdentifier notebookUri) (Position 3 4) Nothing Nothing)
      decl `shouldBe` DeclarationParams (TextDocumentIdentifier shadowUri) (Position 4 4) Nothing Nothing

      sig <- liftIO $ runRequest SMethod_TextDocumentSignatureHelp
        (SignatureHelpParams (TextDocumentIdentifier notebookUri) (Position 3 4) Nothing Nothing)
      sig `shouldBe` SignatureHelpParams (TextDocumentIdentifier shadowUri) (Position 4 4) Nothing Nothing

  describe "the answers" $ do
    it "puts a rename's edit back on the notebook" $ do
      let answered = InL (changesEdit shadowUri [TextEdit (lineRange 4) "newName"])
      transformed <- liftIO $ runResponse SMethod_TextDocumentRename
        (RenameParams Nothing (TextDocumentIdentifier notebookUri) (Position 3 4) "newName") answered
      transformed `shouldBe` InL (changesEdit notebookUri [TextEdit (lineRange 3) "newName"])

    it "puts the prepareRename range back on the notebook" $ do
      transformed <- liftIO $ runResponse SMethod_TextDocumentPrepareRename
        (PrepareRenameParams (TextDocumentIdentifier notebookUri) (Position 3 4) Nothing)
        (InL (PrepareRenameResult (InL (lineRange 4))))
      transformed `shouldBe` InL (PrepareRenameResult (InL (lineRange 3)))

    it "refuses a rename that would land outside a cell" $ do
      let answered = InL (changesEdit shadowUri [TextEdit (lineRange 0) "newName"])
      transformed <- liftIO $ runResponse SMethod_TextDocumentRename
        (RenameParams Nothing (TextDocumentIdentifier notebookUri) (Position 3 4) "newName") answered
      transformed `shouldBe` InR Null

runRequest :: SMethod (m :: Method 'ClientToServer 'Request) -> MessageParams m -> IO (MessageParams m)
runRequest method params = do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformClientReq' testConfig method params) state

runResponse :: SMethod (m :: Method 'ClientToServer 'Request) -> MessageParams m -> MessageResult m -> IO (MessageResult m)
runResponse method params result = do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' testConfig method params result) state

lineRange :: UInt -> Range
lineRange l = Range (Position l 4) (Position l 8)

changesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
changesEdit u edits = WorkspaceEdit (Just (M.singleton u edits)) Nothing Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
