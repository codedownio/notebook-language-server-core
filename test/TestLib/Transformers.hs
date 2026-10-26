{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Stand-ins for real transformers, and the 'DocumentState' the transforms look them up in.
-- Lives outside @test/Test@ because everything in there is picked up by sandwich-discover and
-- has to export a @tests@.
module TestLib.Transformers (
  LineShift(..)

  , documentState
  , transformerStateWith

  , notebookUri
  , shadowUri
  ) where

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.UUID as UUID
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import UnliftIO.MVar


-- | The projected document has @n@ extra lines at the top, the way a notebook's code ends up
-- inside a generated wrapper.
newtype LineShift = LineShift UInt

instance Transformer LineShift where
  type Params LineShift = UInt
  transformPosition (LineShift n) (Position l c) = Just (Position (l + n) c)
  untransformPosition (LineShift n) (Position l c)
    | l >= n = Just (Position (l - n) c)
    | otherwise = Nothing

instance MonadIO m => ProjectIn m LineShift where
  project n doc = pure (listToDoc (replicate (fromIntegral n) "" <> docToList doc), LineShift n, Right ())

-- | The document contents are empty: nothing that untransforms a position reads them.
documentState :: tx -> DocumentState tx
documentState tx = DocumentState {
  transformer = tx
  , curLines = mempty
  , curLines' = mempty
  , origUri = notebookUri
  , newUri = shadowUri
  , newPath = "/home/main.ipynb.hs"
  , referenceRegex = mkDocRegex "/home/main.ipynb.hs"
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }

-- | A state holding one open document, for the transforms that reach the transformer through
-- 'Control.Monad.Reader.MonadReader' rather than taking it as an argument.
transformerStateWith :: DocumentState tx -> IO (TransformerState tx)
transformerStateWith ds = TransformerState
  <$> newMVar (M.singleton (getUri (origUri ds)) ds)
  <*> newMVar Nothing
  <*> newMVar Nothing

notebookUri :: Uri
notebookUri = Uri "file:///home/main.ipynb"

shadowUri :: Uri
shadowUri = Uri "file:///home/main.ipynb.hs"
