{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Script.Elision where

import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

elisionCharacters :: [Char]
elisionCharacters =
  [ '\x2019' -- RIGHT SINGLE QUOTATION MARK
  , '\x1FBD' -- GREEK KORONIS
  ]

data IsElided = Elided | NotElided deriving (Eq, Ord, Show, Generic)
instance ToJSON IsElided
instance FromJSON IsElided

newtype ElisionChar = ElisionChar { _getElisionChar :: Char } deriving (Eq, Show, Ord, Generic)
makeLenses ''ElisionChar
instance ToJSON ElisionChar
instance FromJSON ElisionChar
