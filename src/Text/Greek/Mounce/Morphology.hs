{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Greek.Mounce.Morphology where

import Control.Lens
import Data.Text (Text)
import Data.Data
import Data.List
import Data.Monoid ((<>))
import Text.Greek.Morphology.Noun
import Text.Greek.Script.Sound
import Text.Greek.Conversions

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
  | AttestedAffix [Sound]
  deriving (Data, Typeable, Show, Eq)

data NounCategory = NounCategory
  { nounDefinition :: Text
  , nounCaseEndings :: NounForms Affix
  , nounWords :: [[Sound]]
  }
  deriving (Show, Eq, Data, Typeable)

nounFormsToCaseNumber :: NounForms a -> [(a, Case, Number)]
nounFormsToCaseNumber x =
  [ (nomSg x, Nominative, Singular)
  , (genSg x, Genitive, Singular)
  , (datSg x, Dative, Singular)
  , (accSg x, Accusative, Singular)
  , (vocSg x, Vocative, Singular)
  , (nomPl x, Nominative, Plural)
  , (genPl x, Genitive, Plural)
  , (datPl x, Dative, Plural)
  , (accPl x, Accusative, Plural)
  , (vocPl x, Vocative, Plural)
  ]

affixToMaybeSounds :: Affix -> Maybe [Sound]
affixToMaybeSounds EmptyAffix = Just []
affixToMaybeSounds UnattestedAffix = Nothing
affixToMaybeSounds (AttestedAffix ss) = Just ss

getMismatches :: NounCategory -> [[Sound]]
getMismatches nc = filter (\w -> not . or . fmap ($ w) . fmap isValid $ validSuffixes) (nounWords nc)
  where
    isValid :: Affix -> [Sound] -> Bool
    isValid (AttestedAffix ss) = isSuffixOf (strip <$> ss) . fmap strip
    isValid EmptyAffix = const True
    isValid UnattestedAffix = const False

    validSuffixes = fmap (\e -> e . nounCaseEndings $ nc) [nomSg, nomPl]
    strip = stripAccent . stripSmoothBreathing

getStem :: NounForms Affix -> [Sound] -> Maybe [Sound]
getStem e w
  | Just nse <- nomSgEnding
  , isSuffixOf nse w
  = Just $ take (length w - length nomSgEnding) w

  | Just npe <- nomPlEnding
  , isSuffixOf npe w
  = Just $ take (length w - length nomPlEnding) w

  | True = Nothing
  where
    nomSgEnding = affixToMaybeSounds . nomSg $ e
    nomPlEnding = affixToMaybeSounds . nomPl $ e

stemToAllNounForms :: NounForms Affix -> [Sound] -> [([Sound], Case, Number)]
stemToAllNounForms nfs s = fmap (& _1 %~ (s ++)) allSuffixes
  where
    allSuffixes = concat $ genForm <$> formsCaseNumber

    genForm :: (Affix, Case, Number) -> [([Sound], Case, Number)]
    genForm = \x -> case affixToMaybeSounds (x ^. _1) of
      Just ss -> [x & _1 .~ ss]
      Nothing -> []

    formsCaseNumber = nounFormsToCaseNumber nfs

affixToString :: Affix -> String
affixToString EmptyAffix = "-"
affixToString UnattestedAffix = "*"
affixToString (AttestedAffix ss) = soundsToString ss
