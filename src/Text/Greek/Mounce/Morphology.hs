{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Greek.Mounce.Morphology where

import Data.Text (Text)
import Data.Data
import Data.List
import Data.Monoid ((<>))

data NounForms a = NounForms
  { nomSg :: a, nomPl :: a
  , genSg :: a, genPl :: a
  , datSg :: a, datPl :: a
  , accSg :: a, accPl :: a
  , vocSg :: a, vocPl :: a
  }
  deriving (Data, Typeable)

deriving instance (Eq a) => Eq (NounForms a)
deriving instance (Show a) => Show (NounForms a)

instance Functor NounForms where
  fmap f (NounForms f0 f1 f2 f3 f4 f5 f6 f7 f8 f9) = NounForms (f f0) (f f1) (f f2) (f f3) (f f4) (f f5) (f f6) (f f7) (f f8) (f f9)

instance Foldable NounForms where
  foldMap f (NounForms f0 f1 f2 f3 f4 f5 f6 f7 f8 f9) = f f0 <> f f1 <> f f2 <> f f3 <> f f4 <> f f5 <> f f6 <> f f7 <> f f8 <> f f9

data Affix
  = EmptyAffix
  | UnattestedAffix
  | AttestedAffix Text
  deriving (Data, Typeable, Show, Eq)

data NounCategory = NounCategory
  { nounDefinition :: Text
  , nounActualCaseEndings :: NounForms Affix
  , nounWords :: [Text]
  }
  deriving (Show, Eq)

getStem :: NounForms String -> String -> Maybe String
getStem e w
  | isSuffixOf nomSgEnding w = Just $ take (length w - length nomSgEnding) w
  | True = Nothing
  where nomSgEnding = nomSg e

stemToAllNounForms :: NounForms String -> String -> NounForms String
stemToAllNounForms e s = fmap (s ++) e
