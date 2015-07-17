module Data.Unicode.MarkedLetter where

import Data.Char
import Data.Foldable (foldl')

data MarkedLetter = MarkedLetter
  { letter :: Char
  , marks :: [Char]
  }
  deriving (Eq, Show)

data GroupMarksResult = GroupMarksResult
  { skippedChars :: [Char]
  , markedLetters :: [MarkedLetter]
  }
  deriving (Eq, Show)

emptyResult :: GroupMarksResult
emptyResult = GroupMarksResult [] []

groupMarks :: Foldable t => t Char -> GroupMarksResult
groupMarks = foldl' addChar emptyResult

addChar :: GroupMarksResult -> Char -> GroupMarksResult
addChar (GroupMarksResult ss gs) c
  | True <- isLetter c
  = GroupMarksResult ss ((MarkedLetter c []) : gs)

  | True <- isMark c
  , (MarkedLetter el ms) : gs' <- gs
  = GroupMarksResult ss ((MarkedLetter el (c : ms)) : gs')

  | True
  = GroupMarksResult (c : ss) gs
