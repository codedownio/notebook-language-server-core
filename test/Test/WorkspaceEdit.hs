{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.WorkspaceEdit (tests, main) where

import qualified Data.Map as M
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.Util (DocumentState)
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Transformers


shiftedByThree :: DocumentState LineShift
shiftedByThree = documentState (LineShift 3)

tests :: TopSpec
tests = describe "WorkspaceEdit" $ do
  describe "untransformWorkspaceEdit" $ do
    it "rewrites documentChanges to the notebook" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 5) ": Vec<i32>")]
      untransformWorkspaceEdit shiftedByThree before
        `shouldBe` Just (documentChangesEdit notebookUri [InL (TextEdit (lineRange 2) ": Vec<i32>")])

    it "rewrites changes to the notebook" $ do
      let before = changesEdit shadowUri [TextEdit (lineRange 5) ": Vec<i32>"]
      untransformWorkspaceEdit shiftedByThree before
        `shouldBe` Just (changesEdit notebookUri [TextEdit (lineRange 2) ": Vec<i32>"])

    it "rewrites an annotated edit" $
      untransformWorkspaceEdit shiftedByThree (documentChangesEdit shadowUri [annotatedEdit (lineRange 5)])
        `shouldBe` Just (documentChangesEdit notebookUri [annotatedEdit (lineRange 2)])

    it "drops an edit to another document" $ do
      let before = documentChangesEdit (Uri "file:///home/other.ipynb.hs") [InL (TextEdit (lineRange 5) "x")]
      untransformWorkspaceEdit shiftedByThree before `shouldBe` Nothing

    it "drops an edit whose range isn't in a cell" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 1) "x")]
      untransformWorkspaceEdit shiftedByThree before `shouldBe` Nothing

    it "drops the whole edit when one of several edits doesn't map" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 5) "x"), InL (TextEdit (lineRange 1) "y")]
      untransformWorkspaceEdit shiftedByThree before `shouldBe` Nothing

    it "drops a file operation" $ do
      let createFile = InR (InL (CreateFile Nothing (AString @"create") shadowUri Nothing))
      untransformWorkspaceEdit shiftedByThree (WorkspaceEdit Nothing (Just [createFile]) Nothing)
        `shouldBe` Nothing

  describe "workspaceEditUri" $ do
    it "finds the document in documentChanges" $
      workspaceEditUri (documentChangesEdit shadowUri []) `shouldBe` Just shadowUri

    it "finds the document in changes" $
      workspaceEditUri (changesEdit shadowUri []) `shouldBe` Just shadowUri

    it "is Nothing for an edit naming no document" $
      workspaceEditUri (WorkspaceEdit Nothing Nothing Nothing) `shouldBe` Nothing

annotatedEdit :: Range -> (TextEdit |? AnnotatedTextEdit)
annotatedEdit r = InR (AnnotatedTextEdit r ": Vec<i32>" (ChangeAnnotationIdentifier "OutsideWorkspace"))

lineRange :: UInt -> Range
lineRange l = Range (Position l 8) (Position l 8)

documentChangesEdit :: Uri -> [TextEdit |? AnnotatedTextEdit] -> WorkspaceEdit
documentChangesEdit u edits = WorkspaceEdit Nothing (Just [InL textDocumentEdit]) Nothing
  where textDocumentEdit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u (InR Null)) edits

changesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
changesEdit u edits = WorkspaceEdit (Just (M.singleton u edits)) Nothing Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
