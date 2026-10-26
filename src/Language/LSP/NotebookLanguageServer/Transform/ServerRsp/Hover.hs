{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (
  fixupHoverText

  , mkDocRegex

  -- For testing
  , fixupDocumentReferences'
  ) where

import Control.Lens hiding (List)
import Control.Lens.Regex.Text
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.LSP.Protocol.Lens hiding (id, trace)
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Safe
import System.FilePath (takeFileName)
import Text.Regex.PCRE.Light
import Language.LSP.NotebookLanguageServer.Transform.Util
import UnliftIO.MVar


fixupHoverText :: (TransformerMonad tx n, Transformer tx) => Hover -> n Hover
fixupHoverText initialHover = do
  documentsMap <- ask >>= (readMVar . transformerDocuments)
  flip fix (initialHover, M.toList documentsMap) $ \loop args -> case args of
    (paramsInProgress, []) -> return paramsInProgress
    (paramsInProgress, (_, DocumentState {..}):xs) -> do
      let notebookPath = T.pack <$> uriToFilePath origUri
      newParams <- traverseOf contents (fixupDocumentReferences referenceRegex notebookPath transformer) paramsInProgress
      loop (newParams, xs)

type HoverContents = MarkupContent |? (MarkedString |? [MarkedString])

fixupDocumentReferences :: forall n tx. (MonadLogger n, Transformer tx) => Regex -> Maybe Text -> tx -> HoverContents -> n HoverContents
fixupDocumentReferences docRegex notebookPath transformer (InL (MarkupContent k t)) = (InL . MarkupContent k) <$> (fixupDocumentReferences' docRegex notebookPath transformer t)
fixupDocumentReferences docRegex notebookPath transformer (InR markedStrings) = (InR <$>) $ case markedStrings of
  InL ms -> InL <$> transformMarkedString ms
  InR mss -> InR <$> (mapM transformMarkedString mss)
  where
    transformMarkedString :: MarkedString -> n MarkedString
    transformMarkedString (MarkedString (InL t)) = (MarkedString . InL) <$> (fixupDocumentReferences' docRegex notebookPath transformer t)
    transformMarkedString (MarkedString (InR thing)) = do
      t' <- fixupDocumentReferences' docRegex notebookPath transformer (thing ^. value)
      return $ MarkedString $ InR (thing & set value t')

-- | Rewrite where a hover's text names the shadow document, path and position together.
fixupDocumentReferences' :: forall n tx. (MonadLogger n, Transformer tx) => Regex -> Maybe Text -> tx -> Text -> n Text
fixupDocumentReferences' docRegex notebookPath transformer t =
  traverseOf ((regexing docRegex) . groups) transformGroups t

  where
    transformGroups :: [Text] -> n [Text]
    transformGroups orig@[docRef, locator] = return $ case untransformLocator locator of
      Nothing -> orig
      Just locator' -> [maybe docRef (renameTo docRef) notebookPath, locator']
    transformGroups matches = return matches

    -- The regex is built out of a full path or a bare name, depending on the proxy.
    renameTo :: Text -> Text -> Text
    renameTo docRef path
      | "/" `T.isInfixOf` docRef = path
      | otherwise = T.pack (takeFileName (T.unpack path))

    -- @:line:column@ like a compiler, or @#line@ like jdtls, both 1-based.
    untransformLocator :: Text -> Maybe Text
    untransformLocator locator = case T.uncons locator of
      Just (':', rest) | [Just line, Just ch] <- fmap (readMay . T.unpack) (T.splitOn ":" rest) -> do
        Position line' ch' <- untransform line ch
        return [i|:#{line' + 1}:#{ch' + 1}|]
      Just ('#', rest) | Just line <- readMay (T.unpack rest) -> do
        Position line' _ <- untransform line 1
        return ("#" <> T.pack (show (line' + 1)))
      _ -> Nothing

    untransform :: UInt -> UInt -> Maybe Position
    untransform line ch
      | line < 1 || ch < 1 = Nothing
      | otherwise = untransformPosition transformer (Position (line - 1) (ch - 1))

mkDocRegex :: Text -> Regex
mkDocRegex docName = compile (T.encodeUtf8 ("(" <> escapedName <> ")(:\\d+:\\d+|#\\d+)")) [utf8]
  where
    escapedName = docName
      & (\x -> if filePrefix `T.isPrefixOf` x then T.drop (T.length filePrefix) x else x)
      & L.foldl' (.) id [T.replace c ("\\" <> c) | c <- pcreChars]

    filePrefix = "file://"

    pcreChars = [".", "^", "$", "*", "+", "?", "(", ")", "[", "{", "\\", "|"]
