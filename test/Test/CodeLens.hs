{-# LANGUAGE OverloadedStrings #-}

module Test.CodeLens (tests, main) where

import qualified Data.Aeson as A
import Language.LSP.NotebookLanguageServer.Transform.Common
import Language.LSP.Protocol.Types
import Test.Sandwich


tests :: TopSpec
tests = describe "CodeLens" $ do
  describe "lens data stamping" $ do
    it "round-trips the notebook uri and the server's own data" $ do
      let stamped = stampLensData notebookUri (lens_ (Just (A.object ["hls" A..= (1 :: Int)])))
      unstampLensData stamped `shouldBe` Just (notebookUri, lens_ (Just (A.object ["hls" A..= (1 :: Int)])))

    it "round-trips a lens that had no data" $ do
      unstampLensData (stampLensData notebookUri (lens_ Nothing)) `shouldBe` Just (notebookUri, lens_ Nothing)

    it "leaves the range alone" $
      (snd <$> unstampLensData (stampLensData notebookUri (lens_ Nothing)))
        `shouldBe` Just (lens_ Nothing)

    it "is Nothing for a lens we didn't stamp" $ do
      unstampLensData (lens_ Nothing) `shouldBe` Nothing
      unstampLensData (lens_ (Just (A.object ["hls" A..= (1 :: Int)]))) `shouldBe` Nothing

lens_ :: Maybe A.Value -> CodeLens
lens_ d = CodeLens (Range (Position 3 0) (Position 3 5)) Nothing d

notebookUri :: Uri
notebookUri = Uri "file:///home/main.ipynb"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
