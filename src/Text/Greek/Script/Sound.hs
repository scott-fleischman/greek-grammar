{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Sound where

import Control.Lens
import Data.Data
import Data.Set
import Text.Greek.Script.Token

data Sound a =
    ConsonantSound (Consonant a)
  | VowelSound (Vowel a)
  deriving (Eq, Show, Data, Typeable)

data Consonant a =
    Consonant (TokenContext a)
  | RoughBreathing (Sound a)
  deriving (Eq, Show, Data, Typeable)

data Vowel a =
    Vowel (TokenContext a)
  | Diphthong (TokenContext a) (TokenContext a)
  | ImproperDiphthong (TokenContext a)
  deriving (Eq, Show, Data, Typeable)

tokensToSounds :: [TokenContext a] -> [Sound a]
tokensToSounds [] = []
tokensToSounds (t1 : t2 : ts)
  | True <- isDiphthong
  , (Just Rough) <- t2 ^. token . breathing
  = (ConsonantSound $ RoughBreathing diphthongSound) : diphthongSound : (tokensToSounds ts)

  | True <- isDiphthong
  = diphthongSound : (tokensToSounds ts)

  where
    diphthongSound = VowelSound $ Diphthong t1 t2
    isDiphthong = lacksDiaeresis && isDiphthongPair
    lacksDiaeresis = case t2 ^. token . diaeresis of { Nothing -> True ; _ -> False }
    isDiphthongPair = (t1 ^. token . letter, t2 ^. token . letter) `member` diphthongSet

    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokensToSounds (t : ts)
  | True <- isVowel
  , (Just Rough) <- t ^. token . breathing
  = (ConsonantSound $ RoughBreathing singleVowel) : singleVowel : (tokensToSounds ts)

  | True <- isVowel
  = singleVowel : (tokensToSounds ts)

  | True = (ConsonantSound . Consonant $ t) : (tokensToSounds ts)

  where
    isVowel = (t ^. token . letter) `elem` vowels
    singleVowel = singleVowelToSound t

singleVowelToSound :: TokenContext a -> Sound a
singleVowelToSound t
  | (Just IotaSubscript) <- t ^. token . iotaSubscript
  = VowelSound . ImproperDiphthong $ t

  | True
  = VowelSound . Vowel $ t
