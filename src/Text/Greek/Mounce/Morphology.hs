{-# LANGUAGE DeriveDataTypeable #-}

module Text.Greek.Mounce.Morphology where

import Data.Data

data NounCaseEndings = NounCaseEndings
  { nomSg :: String
  , nomPl :: String
  , genSg :: String
  , genPl :: String
  , datSg :: String
  , datPl :: String
  , accSg :: String
  , accPl :: String
  }
  deriving (Data, Typeable, Show, Eq)

data Noun = Noun
  { nounDefinition :: String
  , nounNounCaseEndings :: NounCaseEndings
  , nounWords :: [String]
  , nounWordsException :: [String]
  }
  deriving (Show, Eq)
