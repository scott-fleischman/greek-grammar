{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Grammar where

import Control.Lens (makeLenses)
import Data.Text (Text)

data Source = Source
  { _author :: Text
  , _title :: Text
  , _year :: Int
  }
makeLenses ''Source

data Part =
    Section Text
  | Page Int

data Citation = Citation Source Part

mounceSource :: Source
mounceSource = Source "William D. Mounce" "The Morphology of Biblical Greek" 1994

mounceCitation :: Text -> Citation
mounceCitation = Citation mounceSource . Section

brooksSource :: Source
brooksSource = Source "James A. Brooks, Carlton L. Winbery" "A Morphology of New Testament Greek" 1994

smythSource :: Source
smythSource = Source "Herbert Weird Smyth, Gordon M. Messing" "Greek Grammar" 1956 -- revised by Messing; Copyright 1920 Smyth; Copyright 1956, renewed 1984
