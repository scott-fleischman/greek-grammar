{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Sound where

import Control.Lens
import Data.Data
import Data.Set
import Text.Greek.Script.Token

data Sound =
    ConsonantSound Token
  | RoughBreathingSound Sound
  | SingleVowelSound Token
  | IotaSubscriptVowelSound Token
  | DiphthongSound Token Token
  deriving (Eq, Show, Data, Typeable)

data SoundContext a = SoundContext
  { _sound :: Sound
  , _context1 :: TokenContext a
  , _context2 :: Maybe (TokenContext a)
  }
  deriving (Eq, Show, Data, Typeable)
makeLenses ''SoundContext

tokensToSounds :: [TokenContext a] -> [SoundContext a]
tokensToSounds [] = []
tokensToSounds (t1 : t2 : ts)
  | True <- isDiphthong
  , (Just Rough) <- t2 ^. token . breathing
  = (SoundContext (RoughBreathingSound (diphthongSound ^. sound)) (diphthongSound ^. context1) (diphthongSound ^. context2)) : diphthongSound : (tokensToSounds ts)

  | True <- isDiphthong
  = diphthongSound : (tokensToSounds ts)

  where
    diphthongSound = SoundContext (DiphthongSound (t1 ^. token) (t2 ^. token)) t1 (Just t2)
    isDiphthong = lacksDiaeresis && isDiphthongPair
    lacksDiaeresis = case t2 ^. token . diaeresis of { Nothing -> True ; _ -> False }
    isDiphthongPair = (t1 ^. token . letter, t2 ^. token . letter) `member` diphthongSet

    diphthongSet :: Set (Letter, Letter)
    diphthongSet = fromList diphthongs

tokensToSounds (t : ts)
  | True <- isVowel
  , (Just Rough) <- t ^. token . breathing
  = (SoundContext (RoughBreathingSound (singleVowel ^. sound)) (singleVowel ^. context1) Nothing) : singleVowel : (tokensToSounds ts)

  | True <- isVowel
  = singleVowel : (tokensToSounds ts)

  | True = (SoundContext (ConsonantSound (t ^. token)) t Nothing) : (tokensToSounds ts)

  where
    isVowel = (t ^. token . letter) `elem` vowels
    singleVowel = singleVowelToSound t

singleVowelToSound :: TokenContext a -> SoundContext a
singleVowelToSound t
  | (Just IotaSubscript) <- t ^. token . iotaSubscript
  = SoundContext (IotaSubscriptVowelSound (t ^. token)) t Nothing

  | True
  = SoundContext (SingleVowelSound (t ^. token)) t Nothing
