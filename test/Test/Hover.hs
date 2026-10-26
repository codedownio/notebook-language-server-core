{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hover (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (fixupDocumentReferences', mkDocRegex)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Config
import TestLib.Transformers


tests :: TopSpec
tests = describe "Hover" $ do
  describe "the hover's range" $ do
    it "comes back on the cell's line" $ do
      transformed <- runHover (hoverWith "plain" (Just (lineRange 8)))
      transformed `shouldBe` InL (hoverWith "plain" (Just (lineRange 7)))

    it "is dropped whole when the range isn't a cell's" $ do
      transformed <- runHover (hoverWith "plain" (Just (lineRange 0)))
      transformed `shouldBe` InR Null

  describe "the shadow document in the hover's text" $ do
    it "rewrites a compiler-style path, line and column" $ do
      transformed <- runHover (hoverWith "defined at /home/main.ipynb.hs:9:3" Nothing)
      transformed `shouldBe` InL (hoverWith "defined at /home/main.ipynb:8:3" Nothing)

    it "rewrites a markdown link's path and line" $ do
      transformed <- runHover (hoverWith "Source: *[x](file:///home/main.ipynb.hs#9)*" Nothing)
      transformed `shouldBe` InL (hoverWith "Source: *[x](file:///home/main.ipynb#8)*" Nothing)

    it "leaves a reference to a line that isn't a cell's alone" $ do
      let hov = hoverWith "defined at /home/main.ipynb.hs:1:3" Nothing
      runHover hov >>= (`shouldBe` InL hov)

    it "leaves a mention with no position alone" $ do
      let hov = hoverWith "compiled from /home/main.ipynb.hs" Nothing
      runHover hov >>= (`shouldBe` InL hov)

  -- The regex is built out of a name the client chose, which can hold anything.
  describe "names the regex has to survive" $ do
    it "PCRE metacharacters, with no notebook path to rewrite to" $
      fixupDocumentReferences' (mkDocRegex "ma$in.i?pynb") Nothing (LineShift 1)
        "Defined at: ma$in.i?pynb:3:1" >>= (`shouldBe` "Defined at: ma$in.i?pynb:2:1")

    it "spaces and parentheses in a long path" $ do
      let path = "/home/a b/c (d)/main.ipynb.hs"
      fixupDocumentReferences' (mkDocRegex path) (Just "/home/a b/c (d)/main.ipynb") (LineShift 1)
        ("Defined at: " <> path <> ":3:1")
        >>= (`shouldBe` "Defined at: /home/a b/c (d)/main.ipynb:2:1")

runHover :: MonadIO m => Hover -> m (MessageResult Method_TextDocumentHover)
runHover hov = liftIO $ do
  state <- transformerStateWith (documentState (LineShift 1))
  runNoLoggingT $ runReaderT
    (transformServerRsp' testConfig SMethod_TextDocumentHover params (InL hov)) state
  where params = HoverParams (TextDocumentIdentifier notebookUri) (Position 7 0) Nothing

hoverWith :: Text -> Maybe Range -> Hover
hoverWith t = Hover (InL (MarkupContent MarkupKind_Markdown t))

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 5)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
