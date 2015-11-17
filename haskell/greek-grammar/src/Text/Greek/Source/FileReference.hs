{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Source.FileReference where

import Prelude hiding (getLine)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Control.Lens
import Data.String

newtype Line = Line { _getLine :: Int } deriving (Eq, Ord, Num, Generic)
instance Show Line where show (Line l) = "Line " ++ show l
makeLenses ''Line
instance ToJSON Line
instance FromJSON Line

newtype Column = Column { _getColumn :: Int } deriving (Eq, Ord, Num, Generic)
instance Show Column where show (Column c) = "Column " ++ show c
makeLenses ''Column
instance ToJSON Column
instance FromJSON Column

newtype Path = Path { _getPath :: FilePath } deriving (Eq, Ord, Generic)
instance IsString Path where fromString = Path
instance Show Path where show (Path p) = show p
makeLenses ''Path
instance ToJSON Path
instance FromJSON Path

data LineReference = LineReference
  { _lineReferenceLine :: Line
  , _lineReferenceColumn :: Column
  } deriving (Eq, Ord, Generic)
instance Show LineReference where show (LineReference l c) = "LineReference (" ++ show l ++ ") (" ++ show c ++ ")"
makeLenses ''LineReference
instance ToJSON LineReference
instance FromJSON LineReference

data FileReference = FileReference
  { _fileReferencePath :: Path
  , _fileReferenceBegin :: LineReference
  , _fileReferenceEnd :: LineReference
  } deriving (Eq, Ord, Generic)
instance Show FileReference where show (FileReference p b e) = "FileReference " ++ show p ++ " (" ++ show b ++ ") (" ++ show e ++ ")"
makeLenses ''FileReference
instance ToJSON FileReference
instance FromJSON FileReference

data FileCharReference = FileCharReference
  { _fileCharReferencePath :: Path
  , _fileCharReferenceLine :: LineReference
  } deriving (Eq, Ord, Generic)
instance Show FileCharReference where show (FileCharReference p l) = "FileCharReference " ++ show p ++ " (" ++ show l ++ ")"
makeLenses ''FileCharReference
instance ToJSON FileCharReference
instance FromJSON FileCharReference
