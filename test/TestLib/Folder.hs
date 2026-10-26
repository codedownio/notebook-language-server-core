{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The folder sandwich made for the test being run, where its files go rather than in /tmp.
module TestLib.Folder (
  testFolder
  ) where

import Control.Monad.IO.Class
import GHC.Stack
import Test.Sandwich


testFolder :: (HasCallStack, HasBaseContextMonad context m, MonadIO m) => m FilePath
testFolder = getCurrentFolder >>= \case
  Just dir -> pure dir
  Nothing -> expectationFailure "No test folder: run with test artifacts enabled"
