{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Script.Elision where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Control.Lens as Lens
import qualified Data.Set as Set

elisionCharacters :: Set.Set Char
elisionCharacters = Set.fromList
  [ '\x2019' -- RIGHT SINGLE QUOTATION MARK
  , '\x1FBD' -- GREEK KORONIS
  ]

data Elided = IsElided | NotElided deriving (Eq, Ord, Show, Generic)
instance ToJSON Elided
instance FromJSON Elided

newtype ElisionChar = ElisionChar { _getElisionChar :: Char } deriving (Eq, Show, Ord, Generic)
Lens.makeLenses ''ElisionChar
instance ToJSON ElisionChar
instance FromJSON ElisionChar

type Pair = (Elided, Maybe ElisionChar)

split :: (a -> Char) -> [a] -> (Pair, [a])
split f = Lens.over Lens._2 reverse . getElision . reverse
  where
    getElision (x : xs) | Set.member (f x) elisionCharacters = ((IsElided, Just . ElisionChar . f $ x), xs)
    getElision xs = ((NotElided, Nothing), xs)
