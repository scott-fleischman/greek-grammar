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
  | RoughBreathingSound (SoundP a)
  | SingleVowelSound a
  | IotaSubscriptVowelSound a
  | DiphthongSound a a
  deriving (Eq, Show, Data, Typeable)

instance Functor SoundP where
  fmap f (ConsonantSound x) = ConsonantSound (f x)
  fmap f (RoughBreathingSound x) = RoughBreathingSound (fmap f x)
  fmap f (SingleVowelSound x) = SingleVowelSound (f x)
  fmap f (IotaSubscriptVowelSound x) = IotaSubscriptVowelSound (f x)
  fmap f (DiphthongSound x1 x2) = DiphthongSound (f x1) (f x2)

type Sound = SoundP Token

data SoundContext a = SoundContext
  { _sound :: Sound
  , _context1 :: TokenContext a
  , _context2 :: Maybe (TokenContext a)
  }
  deriving (Eq, Show, Data, Typeable)
makeLenses ''SoundContext

makeSound :: (Token -> Sound) -> TokenContext a -> SoundContext a
makeSound s t = SoundContext (s (t ^. token)) t Nothing

makeDiphthongSound :: TokenContext a -> TokenContext a -> SoundContext a
makeDiphthongSound t1 t2 = SoundContext (DiphthongSound (t1 ^. token) (t2 ^. token)) t1 (Just t2)

makeRoughBreathingSound :: SoundContext a -> SoundContext a
makeRoughBreathingSound s = SoundContext (RoughBreathingSound (s ^. sound)) (s ^. context1) (s ^. context2)

tokensToSounds :: [TokenContext a] -> [SoundContext a]
tokensToSounds [] = []
tokensToSounds (t1 : t2 : ts)
  | True <- isDiphthong
  , (Just Rough) <- t2 ^. token . breathing
  = (makeRoughBreathingSound diphthongSound) : diphthongSound : (tokensToSounds ts)

  | True <- isDiphthong
  = diphthongSound : (tokensToSounds ts)

  where
    diphthongSound = makeDiphthongSound t1 t2
    isDiphthong = lacksDiaeresis && isDiphthongPair
    lacksDiaeresis = case t2 ^. token . diaeresis of { Nothing -> True ; _ -> False }
    isDiphthongPair = (t1 ^. token . letter, t2 ^. token . letter) `member` diphthongSet

    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokensToSounds (t : ts)
  | True <- isVowel
  , (Just Rough) <- t ^. token . breathing
  = (makeRoughBreathingSound singleVowel) : singleVowel : (tokensToSounds ts)

  | True <- isVowel
  = singleVowel : (tokensToSounds ts)

  | True = (makeSound ConsonantSound t) : (tokensToSounds ts)

  where
    isVowel = (t ^. token . letter) `elem` vowels
    singleVowel = singleVowelToSound t

singleVowelToSound :: TokenContext a -> SoundContext a
singleVowelToSound t
  | (Just IotaSubscript) <- t ^. token . iotaSubscript
  = makeSound IotaSubscriptVowelSound t

  | True
  = makeSound SingleVowelSound t
