{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Sound where

import Control.Lens
import Data.Data
import Data.Set
import Text.Greek.Script.Token

data SoundP a =
    ConsonantSound a
  | RoughBreathingSound
  | SingleVowelSound a
  | IotaSubscriptVowelSound a
  | DiphthongSound a a
  deriving (Eq, Show, Data, Typeable, Ord)

instance Functor SoundP where
  fmap f (ConsonantSound x) = ConsonantSound (f x)
  fmap _ RoughBreathingSound = RoughBreathingSound
  fmap f (SingleVowelSound x) = SingleVowelSound (f x)
  fmap f (IotaSubscriptVowelSound x) = IotaSubscriptVowelSound (f x)
  fmap f (DiphthongSound x1 x2) = DiphthongSound (f x1) (f x2)

type Sound = SoundP Token

stripAccent :: Sound -> Sound
stripAccent = fmap (& accent .~ Nothing)

stripBreathing :: Sound -> Sound
stripBreathing = fmap (& breathing .~ Nothing)

stripSmoothBreathing :: Sound -> Sound
stripSmoothBreathing = fmap (& breathing %~ removeSmoothBreathing)

stripDiaeresis :: Sound -> Sound
stripDiaeresis = fmap (& diaeresis .~ Nothing)

toLowerCase :: Sound -> Sound
toLowerCase = fmap (& letterCase .~ Lowercase)

tokensToSounds :: [Token] -> [Sound]
tokensToSounds [] = []
tokensToSounds (t1 : t2 : ts)
  | True <- isDiphthong
  , (Just Rough) <- t2 ^. breathing
  = RoughBreathingSound : (stripBreathing diphthongSound) : (tokensToSounds ts)

  | True <- isDiphthong
  = diphthongSound : (tokensToSounds ts)

  where
    diphthongSound = DiphthongSound t1 t2
    isDiphthong = lacksDiaeresis && isDiphthongPair
    lacksDiaeresis = case t2 ^. diaeresis of { Nothing -> True ; _ -> False }
    isDiphthongPair = (t1 ^. letter, t2 ^. letter) `member` diphthongSet

    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokensToSounds (t : ts)
  | True <- isVowel
  , (Just Rough) <- t ^. breathing
  = RoughBreathingSound : (stripBreathing singleVowel) : (tokensToSounds ts)

  | True <- isVowel
  = singleVowel : (tokensToSounds ts)

  | True = (ConsonantSound t) : (tokensToSounds ts)

  where
    isVowel = (t ^. letter) `elem` vowels
    singleVowel = singleVowelToSound t

singleVowelToSound :: Token -> Sound
singleVowelToSound t
  | (Just IotaSubscript) <- t ^. iotaSubscript
  = IotaSubscriptVowelSound t

  | True
  = SingleVowelSound t

soundToTokens :: Sound -> [Token]
soundToTokens (ConsonantSound x) = [x]
soundToTokens (RoughBreathingSound) = []
soundToTokens (SingleVowelSound x) = [x]
soundToTokens (IotaSubscriptVowelSound x) = [x]
soundToTokens (DiphthongSound x1 x2) = [x1, x2]
