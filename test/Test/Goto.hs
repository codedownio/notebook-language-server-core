{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Test.Goto (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.NotebookLanguageServer.Transform.Util (DocumentState(..))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "Goto" $ do
  -- Without these the client is handed the shadow document's path and tries to open a file it
  -- has never heard of.
  describe "textDocument/definition" $ do
    it "puts a single location back on the notebook" $ do
      transformed <- runDefinition (InL (Definition (InL (location shadowUri 8))))
      transformed `shouldBe` InL (Definition (InL (location notebookUri 7)))

    it "puts a list of locations back" $ do
      transformed <- runDefinition (InL (Definition (InR [location shadowUri 8, location shadowUri 9])))
      transformed `shouldBe` InL (Definition (InR [location notebookUri 7, location notebookUri 8]))

    it "puts links back, naming the whole definition and the name alike" $ do
      transformed <- runDefinition (InR (InL [DefinitionLink (link shadowUri 8 9)]))
      transformed `shouldBe` InR (InL [DefinitionLink (link notebookUri 7 8)])

    it "leaves a location in a document we aren't shadowing alone" $ do
      let elsewhere = InL (Definition (InL (location (Uri "file:///home/other.rs") 8)))
      runDefinition elsewhere >>= (`shouldBe` elsewhere)

    it "drops a location that doesn't map back into a cell" $ do
      transformed <- runDefinition (InL (Definition (InR [location shadowUri 0, location shadowUri 8])))
      transformed `shouldBe` InL (Definition (InR [location notebookUri 7]))

    it "leaves null alone" $
      runDefinition (InR (InR Null)) >>= (`shouldBe` InR (InR Null))

    it "matches the shadow document however the server escaped its path" $ do
      let ds = (documentState (LineShift 1)) { newUri = Uri "file:///home/a%20%28b%29/main.hs" }
      transformed <- liftIO $ runWith ds SMethod_TextDocumentDefinition (positionParams 7)
                       (InL (Definition (InL (location (Uri "file:///home/a (b)/main.hs") 8))))
      transformed `shouldBe` InL (Definition (InL (location notebookUri 7)))

  describe "textDocument/implementation" $
    it "puts two implementations back on the notebook" $ do
      let answered = InR (InL [DefinitionLink (link shadowUri 8 9), DefinitionLink (link shadowUri 12 13)])
      transformed <- liftIO $ run SMethod_TextDocumentImplementation (implementationParams 7) answered
      transformed `shouldBe` InR (InL [DefinitionLink (link notebookUri 7 8), DefinitionLink (link notebookUri 11 12)])

run :: (Show (MessageResult m))
  => SMethod (m :: Method 'ClientToServer 'Request) -> MessageParams m -> MessageResult m -> IO (MessageResult m)
run = runWith (documentState (LineShift 1))

runWith :: (Show (MessageResult m)) => DocumentState LineShift
  -> SMethod (m :: Method 'ClientToServer 'Request) -> MessageParams m -> MessageResult m -> IO (MessageResult m)
runWith ds method params result = do
  state <- transformerStateWith ds
  runNoLoggingT $ runReaderT (transformServerRsp' testConfig method params result) state

runDefinition :: MonadIO m => MessageResult Method_TextDocumentDefinition -> m (MessageResult Method_TextDocumentDefinition)
runDefinition = liftIO . run SMethod_TextDocumentDefinition (positionParams 7)

positionParams :: UInt -> DefinitionParams
positionParams line = DefinitionParams (TextDocumentIdentifier notebookUri) (Position line 0) Nothing Nothing

implementationParams :: UInt -> ImplementationParams
implementationParams line = ImplementationParams (TextDocumentIdentifier notebookUri) (Position line 0) Nothing Nothing

location :: Uri -> UInt -> Location
location u line = Location u (lineRange line)

link :: Uri -> UInt -> UInt -> LocationLink
link u whole name = LocationLink Nothing u (lineRange whole) (lineRange name)

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 5)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
