{-# LANGUAGE DeriveDataTypeable #-}

module Text.Greek.Mounce.Phonology where

import Data.Data
import Data.Text (Text)

data Euphony = Euphony
  { euphonyName :: Text
  , euphonyRules :: [EuphonyRule]
  }
  deriving (Show, Eq)

data EuphonyRule = EuphonyRule
  { euphonyRuleFirst :: Text
  , euphonyRuleSecond :: Text
  , euphonyRuleResult :: Text
  }
  deriving (Data, Typeable, Show, Eq)

