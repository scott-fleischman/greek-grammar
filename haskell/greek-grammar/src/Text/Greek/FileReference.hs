{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Greek.FileReference where

import Prelude hiding (getLine)
import Control.Lens
import Data.String

newtype Line = Line { _getLine :: Int } deriving (Eq, Ord, Num)
instance Show Line where show (Line l) = "Line " ++ show l
makeLenses ''Line

newtype Column = Column { _getColumn :: Int } deriving (Eq, Ord, Num)
instance Show Column where show (Column c) = "Column " ++ show c
makeLenses ''Column

newtype Path = Path { _getPath :: FilePath } deriving (Eq, Ord)
instance IsString Path where fromString = Path
instance Show Path where show (Path p) = show p
makeLenses ''Path

data LineReference = LineReference
  { _lineReferenceLine :: Line
  , _lineReferenceColumn :: Column
  } deriving (Eq, Ord)
instance Show LineReference where show (LineReference l c) = "LineReference (" ++ show l ++ ") (" ++ show c ++ ")"
makeLenses ''LineReference

data FileReference = FileReference
  { _fileReferencePath :: Path
  , _fileReferenceBegin :: LineReference
  , _fileReferenceEnd :: LineReference
  }
instance Show FileReference where show (FileReference p b e) = "FileReference " ++ show p ++ " (" ++ show b ++ ") (" ++ show e ++ ")"
makeLenses ''FileReference

data FileCharReference = FileCharReference
  { _fileCharReferencePath :: Path
  , _fileCharReferenceLine :: LineReference
  } deriving (Eq, Ord)
instance Show FileCharReference where show (FileCharReference p l) = "FileCharReference " ++ show p ++ " (" ++ show l ++ ")"
makeLenses ''FileCharReference
