{-# LANGUAGE DeriveDataTypeable #-}

module Text.Greek.Mounce.Euphony where

import Data.Data

data Euphony = Euphony
  { euphonyName :: String
  , euphonyRules :: [EuphonyRule]
  }
  deriving (Show, Eq)

data EuphonyRule = EuphonyRule
  { euphonyRuleFirst :: String
  , euphonyRuleSecond :: String
  , euphonyRuleResult :: String
  }
  deriving (Data, Typeable, Show, Eq)
