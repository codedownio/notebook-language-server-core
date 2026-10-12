{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LSP.NotebookLanguageServer.Transform.Common where

import Control.Lens hiding (List)
import Data.Maybe
import Language.LSP.Transformer
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens as Lens


-- * Transform

transformRange :: Transformer tx => tx -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (Transformer tx, HasRange a Range) => tx -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition tx)
  >>= traverseOf (range . end) (transformPosition tx)

-- * Untransform

untransformRange :: Transformer tx => tx -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (Transformer tx, HasRange a Range) => tx -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition tx)
  >>= traverseOf (range . end) (untransformPosition tx)

untransformRangedMaybe :: (Transformer tx, HasRange a (Maybe Range)) => tx -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition tx)
  >>= traverseOf (range . _Just . end) (untransformPosition tx)

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the cell; drop individual
-- edits that aren't.
untransformInlayHint :: Transformer tx => tx -> InlayHint -> Maybe InlayHint
untransformInlayHint tx hint =
  hint
    & traverseOf position (untransformPosition tx)
    & fmap (over (textEdits . _Just) (mapMaybe (untransformRanged tx)))

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
