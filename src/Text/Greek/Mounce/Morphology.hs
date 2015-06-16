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
  deriving (Data, Typeable, Eq, Show)

instance Functor NounForms where
  fmap f (NounForms f0 f1 f2 f3 f4 f5 f6 f7 f8 f9) = NounForms (f f0) (f f1) (f f2) (f f3) (f f4) (f f5) (f f6) (f f7) (f f8) (f f9)

instance Foldable NounForms where
  foldMap f (NounForms f0 f1 f2 f3 f4 f5 f6 f7 f8 f9) = f f0 <> f f1 <> f f2 <> f f3 <> f f4 <> f f5 <> f f6 <> f f7 <> f f8 <> f f9

data AdjectiveForms a = AdjectiveForms
  { nomSgMasc :: a, nomSgFem :: a, nomSgNeut :: a
  , genSgMasc :: a, genSgFem :: a, genSgNeut :: a
  , datSgMasc :: a, datSgFem :: a, datSgNeut :: a
  , accSgMasc :: a, accSgFem :: a, accSgNeut :: a
  , vocSgMasc :: a, vocSgFem :: a, vocSgNeut :: a
  , nomPlMasc :: a, nomPlFem :: a, nomPlNeut :: a
  , genPlMasc :: a, genPlFem :: a, genPlNeut :: a
  , datPlMasc :: a, datPlFem :: a, datPlNeut :: a
  , accPlMasc :: a, accPlFem :: a, accPlNeut :: a
  , vocPlMasc :: a, vocPlFem :: a, vocPlNeut :: a
  }
  deriving (Data, Typeable, Eq, Show)

adjective2Forms ::
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  a -> a ->
  AdjectiveForms a
adjective2Forms
  nsmf nsn
  gsmf gsn
  dsmf dsn
  asmf asn
  vsmf vsn
  npmf npn
  gpmf gpn
  dpmf dpn
  apmf apn
  vpmf vpn
  = AdjectiveForms
    nsmf nsmf nsn
    gsmf gsmf gsn
    dsmf dsmf dsn
    asmf asmf asn
    vsmf vsmf vsn
    npmf npmf npn
    gpmf gpmf gpn
    dpmf dpmf dpn
    apmf apmf apn
    vpmf vpmf vpn

data Affix
  = UnattestedAffix
  | AttestedAffix [Sound]
  deriving (Data, Typeable, Show, Eq)

data NounLemma = NounLemma
  { _nounLemmaText :: Text
  , _nounLemmaSounds :: [Sound]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''NounLemma

data NounCategory = NounCategory
  { _nounCategoryName :: Text
  , _nounCategoryEndings :: NounForms Affix
  , _nounCategoryLemmas :: [NounLemma]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''NounCategory

data AdjectiveCategory = AdjectiveCategory
  { _adjectiveCategoryName :: Text
  , _adjectiveCategoryEndings :: AdjectiveForms Affix
  , _adjectiveCategoryLemmas :: [NounLemma]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''AdjectiveCategory

data NounForm = NounForm
  { _nounFormSounds :: [Sound]
  , _nounFormCase :: Case
  , _nounFormNumber :: Number
  , _nounFormCategoryName :: Text
  }
  deriving (Show, Eq)
makeLenses ''NounForm

data AdjectiveForm = AdjectiveForm
  { _adjectiveFormSounds :: [Sound]
  , _adjectiveFormCase :: Case
  , _adjectiveFormNumber :: Number
  , _adjectiveFormGender :: Gender
  , _adjectiveFormCategoryName :: Text
  }
  deriving (Show, Eq)
makeLenses ''AdjectiveForm

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

getMismatches :: NounCategory -> [NounLemma]
getMismatches nc = filter (not . or . hasValidSuffixes) lemmas
  where
    hasValidSuffixes :: NounLemma -> [Bool]
    hasValidSuffixes lemma = fmap ($ sounds) isValidSuffixes
      where
        sounds = lemma ^. nounLemmaSounds

    isValidSuffixes :: [[Sound] -> Bool]
    isValidSuffixes = isValid <$> validSuffixes

    isValid :: Affix -> [Sound] -> Bool
    isValid (AttestedAffix ss) = isSuffixOf (stripEnding <$> ss) . fmap stripEnding
    isValid UnattestedAffix = const False

    validSuffixes :: [Affix]
    validSuffixes = fmap ($ endings) [nomSg, nomPl]

    endings :: NounForms Affix
    endings = nc ^. nounCategoryEndings

    lemmas :: [NounLemma]
    lemmas = nc ^. nounCategoryLemmas

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
    allStems = catMaybes allMaybeStems
    allMaybeStems = getStem (nc ^. nounCategoryEndings) <$> lemmaSounds
    lemmaSounds = _nounLemmaSounds <$> nc ^. nounCategoryLemmas
