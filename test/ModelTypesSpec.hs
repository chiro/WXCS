{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ModelTypesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import ModelTypes

instance Arbitrary Judge where
  arbitrary = elements [Accepted, WrongAnswer, RuntimeError, TimeLimitExceeded,
                        MemoryLimitExceeded, OutputLimitExceeded, CompileError,
                        SubmissionError, Pending]

spec :: Spec
spec = do
  describe "toJudge" $ do
    prop "reverses show" $ \j ->
      toJudge (show j) == j

