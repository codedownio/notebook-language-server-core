{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.WorkspaceEdit (spec) where

import qualified Data.Map as M
import qualified Data.UUID as UUID
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich
import Text.Regex.PCRE.Light (compile)


spec :: TopSpec
spec = describe "WorkspaceEdit" $ do
  describe "untransformWorkspaceEdit" $ do
    it "rewrites documentChanges to the notebook" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 5) ": Vec<i32>")]
      untransformWorkspaceEdit documentState before
        `shouldBe` Just (documentChangesEdit notebookUri [InL (TextEdit (lineRange 2) ": Vec<i32>")])

    it "rewrites changes to the notebook" $ do
      let before = changesEdit shadowUri [TextEdit (lineRange 5) ": Vec<i32>"]
      untransformWorkspaceEdit documentState before
        `shouldBe` Just (changesEdit notebookUri [TextEdit (lineRange 2) ": Vec<i32>"])

    it "rewrites an annotated edit" $
      untransformWorkspaceEdit documentState (documentChangesEdit shadowUri [annotatedEdit (lineRange 5)])
        `shouldBe` Just (documentChangesEdit notebookUri [annotatedEdit (lineRange 2)])

    it "drops an edit to another document" $ do
      let before = documentChangesEdit (Uri "file:///tmp/other-shadow/src/other.rs") [InL (TextEdit (lineRange 5) "x")]
      untransformWorkspaceEdit documentState before `shouldBe` Nothing

    it "drops an edit whose range isn't in a cell" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 1) "x")]
      untransformWorkspaceEdit documentState before `shouldBe` Nothing

    it "drops the whole edit when one of several edits doesn't map" $ do
      let before = documentChangesEdit shadowUri [InL (TextEdit (lineRange 5) "x"), InL (TextEdit (lineRange 1) "y")]
      untransformWorkspaceEdit documentState before `shouldBe` Nothing

    it "drops a file operation" $ do
      let createFile = InR (InL (CreateFile Nothing (AString @"create") shadowUri Nothing))
      untransformWorkspaceEdit documentState (WorkspaceEdit Nothing (Just [createFile]) Nothing)
        `shouldBe` Nothing

  describe "workspaceEditUri" $ do
    it "finds the document in documentChanges" $
      workspaceEditUri (documentChangesEdit shadowUri []) `shouldBe` Just shadowUri

    it "finds the document in changes" $
      workspaceEditUri (changesEdit shadowUri []) `shouldBe` Just shadowUri

    it "is Nothing for an edit naming no document" $
      workspaceEditUri (WorkspaceEdit Nothing Nothing Nothing) `shouldBe` Nothing

-- | Stands in for a real transformer: the projected document has three extra lines at the top,
-- the way a notebook's code ends up inside a generated wrapper.
newtype LineShift = LineShift UInt

instance Transformer LineShift where
  type Params LineShift = UInt
  transformPosition (LineShift n) (Position l c) = Just (Position (l + n) c)
  untransformPosition (LineShift n) (Position l c)
    | l >= n = Just (Position (l - n) c)
    | otherwise = Nothing

documentState :: DocumentState LineShift
documentState = DocumentState {
  transformer = LineShift 3
  , curLines = mempty
  , curLines' = mempty
  , origUri = notebookUri
  , newUri = shadowUri
  , newPath = "/tmp/rust-analyzer-shadow-home/src/KTRfBIUnDKeWcNq.rs"
  , referenceRegex = compile "KTRfBIUnDKeWcNq\\.rs" []
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }

notebookUri :: Uri
notebookUri = Uri "file:///home/main.ipynb"

shadowUri :: Uri
shadowUri = Uri "file:///tmp/rust-analyzer-shadow-home/src/KTRfBIUnDKeWcNq.rs"

annotatedEdit :: Range -> (TextEdit |? AnnotatedTextEdit)
annotatedEdit r = InR (AnnotatedTextEdit r ": Vec<i32>" (ChangeAnnotationIdentifier "OutsideWorkspace"))

lineRange :: UInt -> Range
lineRange l = Range (Position l 8) (Position l 8)

documentChangesEdit :: Uri -> [TextEdit |? AnnotatedTextEdit] -> WorkspaceEdit
documentChangesEdit u edits = WorkspaceEdit Nothing (Just [InL textDocumentEdit]) Nothing
  where textDocumentEdit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u (InR Null)) edits

changesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
changesEdit u edits = WorkspaceEdit (Just (M.singleton u edits)) Nothing Nothing
