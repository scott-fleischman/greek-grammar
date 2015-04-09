{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Phonology.Consonants where

import Control.Lens
import Text.Greek.Grammar

data Consonant

data VocalChords = Voiceless | Voiced

data PartOfMouth = Labial | Velar | Dental | Gutteral

data Fricative = Fricative

data AirFlow =
    Stop
  | Aspirate (Maybe Fricative)
  | Liquid (Maybe Fricative)
  | DoubleConsonant
  | Nasal (Maybe Fricative)
  | Sibilant (Maybe Fricative)

data ConsonantPhoneme = Beta | Gamma | GammaNasal | Digamma | Delta | Zeta | Theta | ConsonantalIota | Kappa | Lambda
  | Mu | Nu | Xi | Pi | Rho | RhoRough | Sigma | Tau | Phi | Chi | Psi | RoughBreathing
  deriving (Eq, Show)


allConsonantPhonemes :: [ConsonantPhoneme]
allConsonantPhonemes = [Beta, Gamma, GammaNasal, Digamma, Delta, Zeta, Theta, ConsonantalIota, Kappa, Lambda,
  Mu, Nu, Xi, Pi, Rho, RhoRough, Sigma, Tau, Phi, Chi, Psi, RoughBreathing]

data VocalChordsClassification = VocalChordsClassification
  { _voiceless :: Cited [ConsonantPhoneme]
  , _voiced :: Cited [ConsonantPhoneme]
  }
makeLenses ''VocalChordsClassification

data PartOfMouthClassification = PartOfMouthClassification
  { _labial :: Cited [ConsonantPhoneme]
  , _lingual :: Cited [ConsonantPhoneme]
  , _velar :: Cited [ConsonantPhoneme]
  , _dental :: Cited [ConsonantPhoneme]
  , _gutteral :: Cited [ConsonantPhoneme]
  }
makeLenses ''PartOfMouthClassification

data AirFlowClassification = AirFlowClassification
  { _stop :: Cited [ConsonantPhoneme]
  , _fricative :: Cited [ConsonantPhoneme]
  , _affricate :: Cited [ConsonantPhoneme]
  , _aspirate :: Cited [ConsonantPhoneme]
  , _liquid :: Cited [ConsonantPhoneme]
  , _nasal :: Cited [ConsonantPhoneme]
  , _sibilant :: Cited [ConsonantPhoneme]
  }
makeLenses ''AirFlowClassification

data ConsonantPhonemeClassification = ConsonantPhonemeClassification
  { _vocalChords :: VocalChordsClassification
  , _partOfMouth :: PartOfMouthClassification
  , _airFlow :: AirFlowClassification
  }
makeLenses ''ConsonantPhonemeClassification

generateTable :: ConsonantPhonemeClassification -> [ConsonantPhoneme] -> [(ConsonantPhoneme, Maybe VocalChords, Maybe PartOfMouth, Maybe AirFlow)]
generateTable c = fmap (generateRow c)

generateRow :: ConsonantPhonemeClassification -> ConsonantPhoneme -> (ConsonantPhoneme, Maybe VocalChords, Maybe PartOfMouth, Maybe AirFlow)
generateRow c p = (p, getVocalChords, getPartOfMouth, Nothing)
  where
    getVocalChords
      | isVocalChords voiceless = Just Voiceless
      | isVocalChords voiced = Just Voiced
      | True = Nothing
    getPartOfMouth
      | isPartOfMouth labial = Just Labial
      | isPartOfMouth velar = Just Velar
      | isPartOfMouth dental = Just Dental
      | isPartOfMouth gutteral = Just Gutteral
      | True = Nothing
    isVocalChords vc = isElem $ vocalChords . vc
    isPartOfMouth pom = isElem $ partOfMouth . pom
    isElem path = p `elem` (c ^. path . item)

mounceConsonants :: ConsonantPhonemeClassification
mounceConsonants = ConsonantPhonemeClassification mounceVocalChords mouncePartOfMouth mounceAirFlow

mounceVocalChords :: VocalChordsClassification
mounceVocalChords = VocalChordsClassification
  { _voiceless = (++)
      <$> (mounce § "11.5" $ [Theta, Kappa, Xi, Pi, Sigma, Tau, Phi, Chi, Psi, RhoRough])
      <*> (mounce § "11.12" $ [RoughBreathing])
  , _voiced = mounce § "11.6" $ [Beta, Gamma, Delta, Zeta, Lambda, Mu, Nu, Rho, ConsonantalIota, Digamma, GammaNasal]
  }

mouncePartOfMouth :: PartOfMouthClassification
mouncePartOfMouth = PartOfMouthClassification
  { _labial = mounce § "11.9" $ [Pi, Beta, Phi, Mu, Psi, Digamma]
  , _lingual = mounce § "11.9a" $ [Lambda, Rho, Nu]
  , _velar = mounce § "11.10" $ [Kappa, Gamma, Chi, Xi]
  , _dental = mounce § "11.11" $ [Tau, Delta, Theta, Zeta] -- When MBG refers to “dentals,” ζ is not included.
  , _gutteral = mounce § "11.12" $ [RoughBreathing]
  }

mounceAirFlow :: AirFlowClassification
mounceAirFlow = AirFlowClassification
  { _stop = mounce § "11.14" $ [Beta, Gamma, Delta, Pi, Kappa, Tau] -- The aspirates (φ χ θ) are sometimes called stops (Square of Stops)
  , _fricative = mounce § "11.15" $ [GammaNasal, Lambda, Mu, Nu, Rho, Sigma, RoughBreathing] -- μ, ν sometimes called a liquid fricative
  , _affricate = mounce § "11.16" $ [Psi, Xi, Zeta]
  , _aspirate = mounce § "11.17" $ [Phi, Theta, Chi, RoughBreathing]
  , _liquid = mounce § "11.18" $ [Lambda, Rho]
  , _nasal = mounce § "11.19" $ [Mu, Nu, GammaNasal]
  , _sibilant = mounce § "11.20" $ [Sigma, ConsonantalIota, Digamma] -- ζ ξ ψ The three double consonants are formed by adding δ (or ι), a velar, or a dental, respectively, to a sigma (§11. 16).
  }


semivowel :: Cited [ConsonantPhoneme]
semivowel = mounce § "11.22" $ [ConsonantalIota, Digamma]

squareOfStops :: Cited [[ConsonantPhoneme]]
squareOfStops = mounce § "12.1" $
--            Orders
-- Voiceless, Voiced, Aspirate
  [ [Pi,      Beta,   Phi]    -- Labial
  , [Kappa,   Gamma,  Chi]    -- Velar     Classes
  , [Tau,     Delta,  Theta]  -- Dental
  ]

mounceTable :: [(ConsonantPhoneme, VocalChords, PartOfMouth, AirFlow)]
mounceTable =
  [ (Beta,            Voiced,     Labial,   Stop)
  , (Gamma,           Voiced,     Velar,    Stop)
  , (GammaNasal,      Voiced,     Velar,    Nasal (Just Fricative))
  , (Digamma,         Voiced,     Velar,    Sibilant Nothing)
  , (Delta,           Voiced,     Labial,   Stop)
  , (Zeta,            Voiced,     Dental,   DoubleConsonant)
  , (Theta,           Voiceless,  Dental,   Aspirate Nothing)
  , (ConsonantalIota, Voiced,     Velar,    Sibilant Nothing)
  , (Kappa,           Voiceless,  Velar,    Stop)
  , (Lambda,          Voiced,     Velar,    Liquid (Just Fricative))
  , (Mu,              Voiced,     Labial,   Nasal (Just Fricative))
  , (Nu,              Voiced,     Dental,   Nasal (Just Fricative))
  , (Xi,              Voiced,     Velar,    DoubleConsonant)
  , (Pi,              Voiceless,  Labial,   Stop)
  , (RhoRough,        Voiceless,  Velar,    Liquid (Just Fricative))
  , (Rho,             Voiced,     Velar,    Liquid (Just Fricative))
  , (Sigma,           Voiceless,  Velar,    Sibilant (Just Fricative))
  , (Tau,             Voiceless,  Dental,   Stop)
  , (Phi,             Voiceless,  Labial,   Aspirate Nothing)
  , (Chi,             Voiced,     Velar,    Aspirate Nothing)
  , (Psi,             Voiced,     Labial,   DoubleConsonant)
  , (RoughBreathing,  Voiceless,  Gutteral, Aspirate (Just Fricative))
  ]
