{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Test.ApplyEdit (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import Language.LSP.NotebookLanguageServer.Transform.ServerReq
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Transformers


tests :: TopSpec
tests = describe "ApplyEdit" $ do
  describe "workspace/applyEdit" $ do
    -- The payload HLS sends for a `y :: Integer` code lens, which the client refused with
    -- "'main.ipynb.hs' isn't open" until this transform existed.
    it "rewrites the shadow document and its lines back to the notebook" $ do
      transformed <- runTransform (applyEdit (changesEdit shadowUri [TextEdit (lineRange 4) "y :: Integer\n"]))
      transformed `shouldBe` applyEdit (changesEdit notebookUri [TextEdit (lineRange 3) "y :: Integer\n"])

    it "rewrites documentChanges the same way" $ do
      transformed <- runTransform (applyEdit (documentChangesEdit shadowUri [TextEdit (lineRange 4) "y :: Integer\n"]))
      transformed `shouldBe` applyEdit (documentChangesEdit notebookUri [TextEdit (lineRange 3) "y :: Integer\n"])

    it "leaves an edit to a document we aren't shadowing alone" $ do
      let untouched = applyEdit (changesEdit (Uri "file:///home/other.hs") [TextEdit (lineRange 4) "x"])
      runTransform untouched >>= (`shouldBe` untouched)

    -- Line 0 of the shadow document is the header the transformer inserted, so there's no cell
    -- line to put it back on. Passing it through leaves the client to refuse it, as before.
    it "leaves an edit that doesn't map back into a cell alone" $ do
      let unmappable = applyEdit (changesEdit shadowUri [TextEdit (lineRange 0) "module Main where\n"])
      runTransform unmappable >>= (`shouldBe` unmappable)

runTransform :: MonadIO m => TRequestMessage Method_WorkspaceApplyEdit -> m (TRequestMessage Method_WorkspaceApplyEdit)
runTransform msg = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerReq SMethod_WorkspaceApplyEdit msg) state

applyEdit :: WorkspaceEdit -> TRequestMessage Method_WorkspaceApplyEdit
applyEdit e = TRequestMessage "2.0" (IdInt 5) SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing e)

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 0)

changesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
changesEdit u edits = WorkspaceEdit (Just (M.singleton u edits)) Nothing Nothing

documentChangesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
documentChangesEdit u edits = WorkspaceEdit Nothing (Just [InL textDocumentEdit]) Nothing
  where textDocumentEdit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u (InR Null)) (fmap InL edits)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
