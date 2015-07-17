module Data.Unicode.LetterMarkGroup where

import Data.Char
import Data.Foldable (foldl')

data LetterMarkGroup = LetterMarkGroup
  { unicodeLetter :: Char
  , unicodeMarks :: [Char]
  }
  deriving (Eq, Show)

data GroupMarksResult = GroupMarksResult
  { skippedChars :: [Char]
  , letterMarkGroups :: [LetterMarkGroup]
  }
  deriving (Eq, Show)

emptyResult :: GroupMarksResult
emptyResult = GroupMarksResult [] []

groupMarks :: Foldable t => t Char -> GroupMarksResult
groupMarks = foldl' addChar emptyResult

addChar :: GroupMarksResult -> Char -> GroupMarksResult
addChar (GroupMarksResult ss gs) c
  | True <- isLetter c
  = GroupMarksResult ss ((LetterMarkGroup c []) : gs)

  | True <- isMark c
  , (LetterMarkGroup el ms) : gs' <- gs
  = GroupMarksResult ss ((LetterMarkGroup el (c : ms)) : gs')

  | True
  = GroupMarksResult (c : ss) gs
