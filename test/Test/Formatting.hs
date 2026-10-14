{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Formatting (tests, main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import qualified Data.UUID as UUID
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Formatting
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich
import Text.Regex.PCRE.Light (compile)


tests :: TopSpec
tests = describe "Formatting" $ do
  describe "untransformFormattingEdits" $ do
    it "passes the server's edits through when the shadow document is a copy" $ do
      let edits = [TextEdit (Range (Position 0 0) (Position 0 3)) "let"]
      untransformFormattingEdits (documentState Identity "x  =  1\n" "x  =  1\n") edits
        `shouldBe` edits

    it "replaces the whole document when the projection has to be undone" $
      untransformFormattingEdits (documentState DropFirstLine "x  =  1\n" "generated\nx  =  1\n")
                                 [TextEdit (lineRange 1 7) "x = 1"]
        `shouldBe` [TextEdit (Range (Position 0 0) (Position 1 0)) "x = 1\n"]

    -- cpp and go are in this position: their transformers don't implement unproject, so there's
    -- nothing to hand back. Those servers set notebookConfigHandlesFormatting to False so the
    -- response never reaches here in the first place.
    it "returns no edits for a transformer that can't unproject" $
      untransformFormattingEdits (documentState NoUnproject "x  =  1\n" "generated\nx  =  1\n")
                                 [TextEdit (lineRange 1 7) "x = 1"]
        `shouldBe` []

    it "returns no edits when the server had none" $
      untransformFormattingEdits (documentState Identity "x  =  1\n" "x  =  1\n") []
        `shouldBe` []

-- | The shadow document is the original, as it is for a plain file.
data Identity = Identity

instance Transformer Identity where
  type Params Identity = ()
  unproject _ = Just
  transformPosition _ = Just
  untransformPosition _ = Just

-- | Stands in for a real transformer: the projected document has a generated line on top, the
-- way a notebook's code ends up inside a wrapper.
data DropFirstLine = DropFirstLine

instance Transformer DropFirstLine where
  type Params DropFirstLine = ()
  unproject _ = Just . Rope.fromText . stripTrailingNewline . Rope.toText . snd . Rope.splitAtLine 1
  transformPosition _ (Position l c) = Just (Position (l + 1) c)
  untransformPosition _ (Position l c)
    | l >= 1 = Just (Position (l - 1) c)
    | otherwise = Nothing

-- | A transformer that never implemented 'unproject', so it gets the class's default.
data NoUnproject = NoUnproject

instance Transformer NoUnproject where
  type Params NoUnproject = ()
  transformPosition _ (Position l c) = Just (Position (l + 1) c)
  untransformPosition _ (Position l c)
    | l >= 1 = Just (Position (l - 1) c)
    | otherwise = Nothing

stripTrailingNewline :: Text -> Text
stripTrailingNewline t = fromMaybe t (T.stripSuffix "\n" t)

documentState :: tx -> Text -> Text -> DocumentState tx
documentState tx orig shadow = DocumentState {
  transformer = tx
  , curLines = Rope.fromText orig
  , curLines' = Rope.fromText shadow
  , origUri = Uri "file:///home/main.ipynb"
  , newUri = shadowUri
  , newPath = "/tmp/shadow-home/KTRfBIUnDKeWcNq.txt"
  , referenceRegex = compile "KTRfBIUnDKeWcNq" []
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }

shadowUri :: Uri
shadowUri = Uri "file:///tmp/shadow-home/KTRfBIUnDKeWcNq.txt"

lineRange :: UInt -> UInt -> Range
lineRange l len = Range (Position l 0) (Position l len)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
