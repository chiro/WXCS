{-# LANGUAGE TemplateHaskell #-}
module ModelTypes where

import Data.List (stripPrefix)
import Data.Time (ZonedTime())

import Database.Persist.TH

import Text.Blaze (ToMarkup(..))

import Utils

data JudgeStatus =
  Accepted
  | WrongAnswer
  | RuntimeError
  | TimeLimitExceeded
  | MemoryLimitExceeded
  | OutputLimitExceeded
  | PresentationError
  | CompileError
  | SubmissionError
  | Pending
  | Running
    deriving (Eq, Ord, Enum, Bounded)

instance Show JudgeStatus where
  show Accepted = "Accepted"
  show WrongAnswer = "Wrong Answer"
  show RuntimeError = "Runtime Error"
  show TimeLimitExceeded = "Time Limit Exceeded"
  show MemoryLimitExceeded = "Memory Limit Exceeded"
  show OutputLimitExceeded = "Output Limit Exceeded"
  show PresentationError = "WA: Presentation Error"
  show CompileError = "Compile Error"
  show SubmissionError = "Submission Error"
  show Pending = "Pending"
  show Running = "Running"

instance Read JudgeStatus where
  readsPrec _ r = case res of
    Nothing -> [(SubmissionError, r)]
    Just res' -> [res']
    where res = foldl (\m st -> case stripPrefix (show st) r of
                          Nothing -> m
                          Just re -> Just (st, re)) Nothing [Accepted ..]

instance ToMarkup JudgeStatus where
  toMarkup = toMarkup . show
  preEscapedToMarkup = preEscapedToMarkup . show

derivePersistField "JudgeStatus"

data JudgeType = Aizu
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance ToMarkup JudgeType where
  toMarkup = toMarkup . show
  preEscapedToMarkup = preEscapedToMarkup . show

derivePersistField "JudgeType"

instance ToMarkup ZonedTime where
  toMarkup = toMarkup . showTime
  preEscapedToMarkup = preEscapedToMarkup . showTime

data Languages =
  C
  | Cpp
  | Cpp11
  | Java
  | Csharp
  | D
  | Ruby
  | Python
  | Php
  | Javascript
  deriving (Eq, Ord, Enum, Bounded)

instance Show Languages where
  show C = "C"
  show Cpp = "C++"
  show Cpp11 = "C++11"
  show Java = "JAVA"
  show Csharp = "C#"
  show D = "D"
  show Ruby = "Ruby"
  show Python = "Python"
  show Php = "PHP"
  show Javascript = "JavaScript"

instance Read Languages where
  readsPrec _ r = case res of
    Nothing -> [(Cpp, r)] -- default value
    Just res' -> [res']
    where res = foldl (\m lang -> case stripPrefix (show lang) r of
                          Nothing -> m
                          Just re -> Just (lang, re)) Nothing [C ..]

derivePersistField "Languages"
