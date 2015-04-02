{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Sound where

import Control.Lens
import Data.Set
import Text.Greek.Script.Token

data Sound a =
    ConsonantSound (Consonant a)
  | VowelSound (Vowel a)

data Consonant a =
    Consonant (TokenContext a)
  | RoughBreathing (TokenContext a)

data Vowel a =
    Vowel (TokenContext a)
  | Diphthong (TokenContext a) (TokenContext a)
  | ImproperDiphthong (TokenContext a)

tokenToSound :: [TokenContext a] -> [Sound a]
tokenToSound [] = []
tokenToSound (t1 : t2 : ts)
  | el1 <- t1 ^. token . letter
  , el2 <- t2 ^. token . letter
  , Nothing <- t2 ^. token . diaeresis
  , True <- (el1, el2) `member` diphthongSet
  = (VowelSound $ Diphthong t1 t2) : (tokenToSound ts)

  where
    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokenToSound (t : ts)
  | (t ^. token . letter) `elem` vowels = (vowelToSounds t) ++ (tokenToSound ts)
  | True = (ConsonantSound . Consonant $ t) : (tokenToSound ts)

vowelToSounds :: TokenContext a -> [Sound a]
vowelToSounds t
  | (Just IotaSubscript) <- t ^. token . iotaSubscript
  = [VowelSound . ImproperDiphthong $ t]

  | (Just Rough) <- t ^. token . breathing
  = [ConsonantSound . RoughBreathing $ t, VowelSound . Vowel $ t]

  | True
  = [VowelSound . Vowel $ t]
