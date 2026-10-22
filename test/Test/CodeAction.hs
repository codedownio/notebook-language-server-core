{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.CodeAction (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "CodeAction" $ do
  describe "textDocument/codeAction" $ do
    -- rust-analyzer answers "Insert explicit type" with the edit filled in, in the shadow
    -- document's coordinates. Without this the insert lands a line below the one asked about.
    it "puts an inline edit back on the notebook's line" $ do
      let answered = [InR (action (Just (documentChangesEdit shadowUri [InL (TextEdit (lineRange 8) ": f64")])))]
      transformed <- runTransform (askedAbout 7) (InL answered)
      transformed `shouldBe` InL [InR (action (Just (documentChangesEdit notebookUri [InL (TextEdit (lineRange 7) ": f64")])))]

    it "leaves an action with no edit for resolve to fill in" $ do
      let answered = [InR (action Nothing)]
      runTransform (askedAbout 7) (InL answered) >>= (`shouldBe` InL answered)

    it "passes a plain command through" $ do
      let answered = [InL (Command "Run" "rust-analyzer.run" Nothing)]
      runTransform (askedAbout 7) (InL answered) >>= (`shouldBe` InL answered)

    it "leaves a null answer alone" $
      runTransform (askedAbout 7) (InR Null) >>= (`shouldBe` InR Null)

runTransform :: MonadIO m
  => MessageParams Method_TextDocumentCodeAction
  -> MessageResult Method_TextDocumentCodeAction
  -> m (MessageResult Method_TextDocumentCodeAction)
runTransform params result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' testConfig SMethod_TextDocumentCodeAction params result) state

askedAbout :: UInt -> MessageParams Method_TextDocumentCodeAction
askedAbout line = CodeActionParams Nothing Nothing (TextDocumentIdentifier notebookUri) (lineRange line)
                    (CodeActionContext [] Nothing Nothing)

action :: Maybe WorkspaceEdit -> CodeAction
action e = CodeAction "Insert explicit type `f64`" (Just CodeActionKind_QuickFix) Nothing Nothing Nothing
             e Nothing Nothing

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 0)

documentChangesEdit :: Uri -> [TextEdit |? AnnotatedTextEdit] -> WorkspaceEdit
documentChangesEdit u edits = WorkspaceEdit Nothing
  (Just [InL (TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u (InR Null)) edits)]) Nothing

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
