{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Text.Greek.Mounce.Morphology where

import Control.Lens
import Data.Text (Text)
import Data.Data
import Data.Maybe (catMaybes, isNothing)
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

instance Foldable AdjectiveForms where
  foldMap f (AdjectiveForms
    f0  f1  f2  f3  f4  f5  f6  f7  f8  f9
    f10 f11 f12 f13 f14 f15 f16 f17 f18 f19
    f20 f21 f22 f23 f24 f25 f26 f27 f28 f29)
      =  f f0  <> f f1  <> f f2  <> f f3  <> f f4  <> f f5  <> f f6  <> f f7  <> f f8  <> f f9 
      <> f f10 <> f f11 <> f f12 <> f f13 <> f f14 <> f f15 <> f f16 <> f f17 <> f f18 <> f f19
      <> f f20 <> f f21 <> f f22 <> f f23 <> f f24 <> f f25 <> f f26 <> f f27 <> f f28 <> f f29

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

data Lemma = Lemma
  { _lemmaText :: Text
  , _lemmaSounds :: [Sound]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''Lemma

data NounCategory = NounCategory
  { _nounCategoryName :: Text
  , _nounCategoryEndings :: NounForms Affix
  , _nounCategoryLemmas :: [Lemma]
  }
  deriving (Show, Eq, Data, Typeable)
makeLenses ''NounCategory

data AdjectiveCategory = AdjectiveCategory
  { _adjectiveCategoryName :: Text
  , _adjectiveCategoryEndings :: AdjectiveForms Affix
  , _adjectiveCategoryLemmas :: [Lemma]
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

adjectiveFormsToCaseNumberGender :: AdjectiveForms a -> [(a, Case, Number, Gender)]
adjectiveFormsToCaseNumberGender x =
  [ (nomSgMasc x, Nominative, Singular, Masculine)
  , (genSgMasc x, Genitive, Singular, Masculine)
  , (datSgMasc x, Dative, Singular, Masculine)
  , (accSgMasc x, Accusative, Singular, Masculine)
  , (vocSgMasc x, Vocative, Singular, Masculine)
  , (nomPlMasc x, Nominative, Plural, Masculine)
  , (genPlMasc x, Genitive, Plural, Masculine)
  , (datPlMasc x, Dative, Plural, Masculine)
  , (accPlMasc x, Accusative, Plural, Masculine)
  , (vocPlMasc x, Vocative, Plural, Masculine)
  , (nomSgFem x, Nominative, Singular, Feminine)
  , (genSgFem x, Genitive, Singular, Feminine)
  , (datSgFem x, Dative, Singular, Feminine)
  , (accSgFem x, Accusative, Singular, Feminine)
  , (vocSgFem x, Vocative, Singular, Feminine)
  , (nomPlFem x, Nominative, Plural, Feminine)
  , (genPlFem x, Genitive, Plural, Feminine)
  , (datPlFem x, Dative, Plural, Feminine)
  , (accPlFem x, Accusative, Plural, Feminine)
  , (vocPlFem x, Vocative, Plural, Feminine)
  , (nomSgNeut x, Nominative, Singular, Neuter)
  , (genSgNeut x, Genitive, Singular, Neuter)
  , (datSgNeut x, Dative, Singular, Neuter)
  , (accSgNeut x, Accusative, Singular, Neuter)
  , (vocSgNeut x, Vocative, Singular, Neuter)
  , (nomPlNeut x, Nominative, Plural, Neuter)
  , (genPlNeut x, Genitive, Plural, Neuter)
  , (datPlNeut x, Dative, Plural, Neuter)
  , (accPlNeut x, Accusative, Plural, Neuter)
  , (vocPlNeut x, Vocative, Plural, Neuter)
  ]

stripEnding :: Sound -> Sound
stripEnding = toLowerCase . stripAccent . stripSmoothBreathing

nounFormsLemmaSuffixes :: NounForms Affix -> [Affix]
nounFormsLemmaSuffixes x = fmap ($ x) [nomSg, nomPl]

nounCategoryLemmaSuffixes :: NounCategory -> [Affix]
nounCategoryLemmaSuffixes (NounCategory _ e _) = nounFormsLemmaSuffixes e

adjectiveFormsLemmaSuffixes :: AdjectiveForms Affix -> [Affix]
adjectiveFormsLemmaSuffixes x = fmap ($ x) [nomSgMasc, nomPlMasc]

adjectiveCategoryLemmaSuffixes :: AdjectiveCategory -> [Affix]
adjectiveCategoryLemmaSuffixes (AdjectiveCategory _ e _) = adjectiveFormsLemmaSuffixes e

getNounMismatches :: NounCategory -> [Lemma]
getNounMismatches (NounCategory _ endings lemmas) = getMismatches (nounFormsLemmaSuffixes endings) lemmas

getMismatches :: [Affix] -> [Lemma] -> [Lemma]
getMismatches suffixes = filter (isNothing . tryGetStemFromAffixes suffixes . _lemmaSounds)

tryRemovePrefix :: [Sound] -> Affix -> Maybe [Sound]
tryRemovePrefix ss a
  | Just e <- affixToMaybeSounds a
  , isSuffixOf e ss
  = Just $ removeSuffix e (fmap stripEnding ss)

  | True = Nothing

tryGetStemFromAffixes :: [Affix] -> [Sound] -> Maybe [Sound]
tryGetStemFromAffixes as ls = case catMaybes . fmap (tryRemovePrefix strippedLemma) $ as of
  x : _ -> Just x
  _ -> Nothing
  where
    strippedLemma = stripEnding <$> ls

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
    allMaybeStems = tryGetStemFromAffixes (nounCategoryLemmaSuffixes nc) <$> nounLemmaSounds
    nounLemmaSounds = _lemmaSounds <$> nc ^. nounCategoryLemmas
