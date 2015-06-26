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

data Cited a = Cited
  { _citations :: [Citation]
  , _item :: a
  }
makeLenses ''Cited

instance Functor Cited where
  fmap f (Cited cs i) = Cited cs (f i)

instance Applicative Cited where
  pure a = Cited [] a
  (Cited cs f) <*> (Cited cs' a) = Cited (cs ++ cs') (f a)

(§) :: Source -> Text -> a -> Cited a
s § t = Cited [Citation s . Section $ t]

(§§) :: Source -> [Text] -> a -> Cited a
s §§ ts = Cited (Citation s . Section <$> ts)

mounce :: Source
mounce = Source "William D. Mounce" "The Morphology of Biblical Greek" 1994

brooksWinbery :: Source
brooksWinbery = Source "James A. Brooks, Carlton L. Winbery" "A Morphology of New Testament Greek" 1994

smyth :: Source
smyth = Source "Herbert Weird Smyth, Gordon M. Messing" "Greek Grammar" 1956 -- revised by Messing; Copyright 1920 Smyth; Copyright 1956, renewed 1984
