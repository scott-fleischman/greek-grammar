module Text.Greek.FileReference where

import Prelude hiding (getLine)
import Data.String

newtype Line = Line { getLine :: Int } deriving (Eq, Ord)
instance Show Line where show (Line l) = "Line " ++ show l

newtype Column = Column { getColumn :: Int } deriving (Eq, Ord)
instance Show Column where show (Column c) = "Column " ++ show c

newtype Path = Path { getPath :: FilePath } deriving (Eq, Ord)
instance IsString Path where fromString = Path
instance Show Path where show (Path p) = show p

data LineReference = LineReference
  { lineReferenceLine :: Line
  , lineReferenceColumn :: Column }
instance Show LineReference where show (LineReference l c) = "LineReference (" ++ show l ++ ") (" ++ show c ++ ")"

data FileReference = FileReference
  { fileReferencePath :: Path
  , fileReferenceBegin :: LineReference
  , fileReferenceEnd :: LineReference }
instance Show FileReference where show (FileReference p b e) = "FileReference " ++ show p ++ " (" ++ show b ++ ") (" ++ show e ++ ")"
