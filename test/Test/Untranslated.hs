{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Untranslated (tests, main) where

import Control.Monad (replicateM_, void)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ClientReq
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers
import UnliftIO.MVar


type Run = NotebookConfig LineShift -> ReaderT (TransformerState LineShift) (LoggingT IO) ()

tests :: TopSpec
tests = describe "A method with no arm" $ do
  -- foldingRange has no arm, which is the point: it stands in for whatever is added next
  it "says which method it was" $ do
    warnings <- liftIO $ warningsFrom $ \config ->
      void $ transformClientReq config SMethod_TextDocumentFoldingRange (foldingRangeOn notebookUri)
    any ("FoldingRange" `T.isInfixOf`) warnings `shouldBe` True
    any ("client request" `T.isInfixOf`) warnings `shouldBe` True

  it "says it once, however many times the method comes past" $ do
    warnings <- liftIO $ warningsFrom $ \config ->
      replicateM_ 5 (void $ transformClientReq config SMethod_TextDocumentFoldingRange (foldingRangeOn notebookUri))
    length warnings `shouldBe` 1

  it "says it again for a different method" $ do
    warnings <- liftIO $ warningsFrom $ \config -> do
      void $ transformClientReq config SMethod_TextDocumentFoldingRange (foldingRangeOn notebookUri)
      void $ transformClientReq config SMethod_TextDocumentDocumentLink (documentLinkOn notebookUri)
    length warnings `shouldBe` 2

  it "keeps quiet about a method that has an arm" $ do
    warnings <- liftIO $ warningsFrom $ \config ->
      void $ transformClientReq config SMethod_TextDocumentHover hoverOnNotebook
    warnings `shouldBe` []

-- | Run something against a state holding one open document, collecting the warnings it logs.
warningsFrom :: Run -> IO [Text]
warningsFrom action = do
  state <- transformerStateWith (documentState (LineShift 1))
  collected <- newMVar []
  let logger _loc _src level msg = case level of
        LevelWarn -> modifyMVar_ collected (return . (<> [decodeUtf8 (fromLogStr msg)]))
        _ -> return ()
  runLoggingT (runReaderT (action testConfig) state) logger
  readMVar collected

hoverOnNotebook :: TRequestMessage Method_TextDocumentHover
hoverOnNotebook = TRequestMessage "2.0" (IdInt 1) SMethod_TextDocumentHover
                    (HoverParams (TextDocumentIdentifier notebookUri) (Position 3 0) Nothing)

foldingRangeOn :: Uri -> TRequestMessage Method_TextDocumentFoldingRange
foldingRangeOn u = TRequestMessage "2.0" (IdInt 1) SMethod_TextDocumentFoldingRange
                     (FoldingRangeParams Nothing Nothing (TextDocumentIdentifier u))

documentLinkOn :: Uri -> TRequestMessage Method_TextDocumentDocumentLink
documentLinkOn u = TRequestMessage "2.0" (IdInt 1) SMethod_TextDocumentDocumentLink
                     (DocumentLinkParams Nothing Nothing (TextDocumentIdentifier u))

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
