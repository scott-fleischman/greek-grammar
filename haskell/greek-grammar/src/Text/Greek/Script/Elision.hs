{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Elision where

import Control.Lens

elisionCharacters :: [Char]
elisionCharacters =
  [ '\x2019' -- RIGHT SINGLE QUOTATION MARK
  , '\x1FBD' -- GREEK KORONIS
  ]

data Elision = Elision deriving (Eq, Ord, Show)
data IsElided = Elided | NotElided deriving (Eq, Ord, Show)

newtype ElisionChar = ElisionChar { _getElisionChar :: Char } deriving (Eq, Show, Ord)
makeLenses ''ElisionChar
