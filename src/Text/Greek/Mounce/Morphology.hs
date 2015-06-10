{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Text.Greek.Mounce.Morphology where

import Control.Lens
import Data.Text (Text)
import Data.Data
import Data.Maybe (catMaybes)
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
  = UnattestedAffix
  | AttestedAffix [Sound]
  deriving (Data, Typeable, Show, Eq)

data NounCategory = NounCategory
  { _nounCategoryName :: Text
  , _nounCategoryEndings :: NounForms Affix
  , _nounCategoryWords :: [[Sound]]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''NounCategory

data NounForm = NounForm
  { _nounFormSounds :: [Sound]
  , _nounFormCase :: Case
  , _nounFormNumber :: Number
  , _nounFormCategoryName :: Text
  }
  deriving (Show, Eq)
makeLenses ''NounForm

affixToString :: Affix -> String
affixToString UnattestedAffix = "*"
affixToString (AttestedAffix []) = "-"
affixToString (AttestedAffix ss@(_:_)) = soundsToString ss

affixToMaybeSounds :: Affix -> Maybe [Sound]
affixToMaybeSounds UnattestedAffix = Nothing
affixToMaybeSounds (AttestedAffix ss) = Just ss

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

stripEnding :: Sound -> Sound
stripEnding = toLowerCase . stripAccent . stripSmoothBreathing

getMismatches :: NounCategory -> [[Sound]]
getMismatches nc = filter (\w -> not . or . fmap ($ w) . fmap isValid $ validSuffixes) (nc ^. nounCategoryWords)
  where
    isValid :: Affix -> [Sound] -> Bool
    isValid (AttestedAffix ss) = isSuffixOf (stripEnding <$> ss) . fmap stripEnding
    isValid UnattestedAffix = const False

    validSuffixes = fmap ($ (nc ^. nounCategoryEndings)) [nomSg, nomPl]

getStem :: NounForms Affix -> [Sound] -> Maybe [Sound]
getStem e w
  | Just nse <- nomSgEnding
  , isSuffixOf nse strippedWord
  = Just $ removeSuffix nse strippedWord

  | Just npe <- nomPlEnding
  , isSuffixOf npe strippedWord
  = Just $ removeSuffix npe strippedWord

  | True = Nothing
  where
    strippedWord = stripEnding <$> w
    nomSgEnding = affixToMaybeSounds . nomSg $ e
    nomPlEnding = affixToMaybeSounds . nomPl $ e

removeSuffix :: [a] -> [a] -> [a]
removeSuffix ss xs = take (length xs - length ss) xs

stemToAllAttestedForms :: Text -> NounForms Affix -> [Sound] -> [NounForm]
stemToAllAttestedForms d nfs s = fmap (& nounFormSounds %~ (s ++)) allSuffixes
  where
    allSuffixes = concat $ makeSuffix <$> formsCaseNumber

    makeSuffix :: (Affix, Case, Number) -> [NounForm]
    makeSuffix = \x -> case affixToMaybeSounds (x ^. _1) of
      Just ss -> [NounForm ss (x ^. _2) (x ^. _3) d]
      Nothing -> []

    formsCaseNumber = nounFormsToCaseNumber nfs

nounCategoryToAllForms :: NounCategory -> [NounForm]
nounCategoryToAllForms nc = concat . fmap applyAllEndings $ allStems
  where
    applyAllEndings  = stemToAllAttestedForms (nc ^. nounCategoryName) (nc ^. nounCategoryEndings)
    allStems = catMaybes $ getStem (nc ^. nounCategoryEndings) <$> nc ^. nounCategoryWords
