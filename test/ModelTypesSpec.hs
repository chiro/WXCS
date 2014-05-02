{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ModelTypesSpec where

import ModelTypes
import SpecHelper

instance Arbitrary JudgeStatus where
  arbitrary = elements [Accepted, WrongAnswer, RuntimeError, TimeLimitExceeded,
                        MemoryLimitExceeded, OutputLimitExceeded, CompileError,
                        PresentationError, SubmissionError, Pending]

instance Arbitrary Languages where
  arbitrary = elements [C, Cpp, Cpp11, Java, Csharp, D, Ruby, Python, Php, Javascript]

spec :: Spec
spec = do
  describe "JudgeStatus" $ do
    prop "read reverses show" $ \j ->
      (read (show j) :: JudgeStatus) == j

  describe "Languages" $ do
    prop "read reverses show" $ \j ->
      (read (show j) :: Languages) == j
