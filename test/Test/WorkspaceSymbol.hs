{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Test.WorkspaceSymbol (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


-- A workspace query carries no document of its own, so a hit is put back by the shadow uri it
-- came with. Without that the client is told to open a file it has never heard of.
tests :: TopSpec
tests = describe "workspace/symbol" $ do
  it "puts a hit back on the notebook" $ do
    transformed <- run (InL [symbolInformation "alpha" shadowUri 8])
    transformed `shouldBe` InL [symbolInformation "alpha" notebookUri 7]

  it "puts a workspace symbol's location back too" $ do
    transformed <- run (InR (InL [workspaceSymbol "alpha" (InL (location shadowUri 8))]))
    transformed `shouldBe` InR (InL [workspaceSymbol "alpha" (InL (location notebookUri 7))])

  it "swaps the uri of a hit that came without a range" $ do
    transformed <- run (InR (InL [workspaceSymbol "alpha" (InR (LocationUriOnly shadowUri))]))
    transformed `shouldBe` InR (InL [workspaceSymbol "alpha" (InR (LocationUriOnly notebookUri))])

  it "leaves a hit in a document we aren't shadowing alone" $ do
    let elsewhere = InL [symbolInformation "alpha" (Uri "file:///home/other.hs") 8]
    run elsewhere >>= (`shouldBe` elsewhere)

  it "drops a hit that doesn't map back into a cell" $ do
    transformed <- run (InL [symbolInformation "alpha" shadowUri 0, symbolInformation "alpha" shadowUri 8])
    transformed `shouldBe` InL [symbolInformation "alpha" notebookUri 7]

  it "leaves null alone" $
    run (InR (InR Null)) >>= (`shouldBe` InR (InR Null))

  -- The wrapper's own symbols are no more worth listing here than in a document's outline
  it "leaves out a symbol the language says to ignore" $ do
    let hits = InL [symbolInformation "main" shadowUri 8, symbolInformation "alpha" shadowUri 9]
    transformed <- runWith (testConfig { notebookConfigIgnoreSymbol = (== "main") }) hits
    transformed `shouldBe` InL [symbolInformation "alpha" notebookUri 8]

run :: MonadIO m => MessageResult Method_WorkspaceSymbol -> m (MessageResult Method_WorkspaceSymbol)
run = runWith testConfig

runWith :: MonadIO m => NotebookConfig LineShift -> MessageResult Method_WorkspaceSymbol -> m (MessageResult Method_WorkspaceSymbol)
runWith config result = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT (transformServerRsp' config SMethod_WorkspaceSymbol params result) state
  where params = WorkspaceSymbolParams Nothing Nothing "alpha"

symbolInformation :: Text -> Uri -> UInt -> SymbolInformation
symbolInformation symbolName u line =
  SymbolInformation symbolName SymbolKind_Function Nothing Nothing Nothing (location u line)

workspaceSymbol :: Text -> (Location |? LocationUriOnly) -> WorkspaceSymbol
workspaceSymbol symbolName loc = WorkspaceSymbol symbolName SymbolKind_Function Nothing Nothing loc Nothing

location :: Uri -> UInt -> Location
location u line = Location u (Range (Position line 0) (Position line 5))

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
