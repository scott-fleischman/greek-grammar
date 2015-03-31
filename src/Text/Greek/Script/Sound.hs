{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Script.Sound where

import Control.Lens
import Data.List
import Data.Set
import Text.Greek.Script.Token

data Sound =
    ConsonantSound Consonant
  | VowelSound Vowel

data Consonant =
    Consonant Token
  | RoughBreathing Token

data Vowel =
    Vowel Token
  | Diphthong Token Token
  | ImproperDiphthong Token

tokenToSound :: [Token] -> [Sound]
tokenToSound [] = []
tokenToSound (t1 : t2 : ts)
  | el1 <- t1 ^. letter
  , el2 <- t2 ^. letter
  , True <- (el1, el2) `member` diphthongSet
  = (VowelSound $ Diphthong t1 t2) : (tokenToSound ts)

  where
    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokenToSound (t : ts)
  | (t ^. letter) `elem` vowels = (vowelToSounds t) ++ (tokenToSound ts)
  | True = (ConsonantSound . Consonant $ t) : (tokenToSound ts)

vowelToSounds :: Token -> [Sound]
vowelToSounds t
  | (Just IotaSubscript) <- t ^. iotaSubscript
  = [VowelSound . ImproperDiphthong $ t]

  | (Just Rough) <- t ^. breathing
  = [ConsonantSound . RoughBreathing $ t, VowelSound . Vowel $ t]

  | True
  = [VowelSound . Vowel $ t]
