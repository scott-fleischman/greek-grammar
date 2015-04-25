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
  , vocSg :: String
  , vocPl :: String
  }
  deriving (Data, Typeable, Show, Eq)

data Noun = Noun
  { nounDefinition :: String
  , nounNounCaseEndings :: NounCaseEndings
  , nounWords :: [String]
  }
  deriving (Show, Eq)
