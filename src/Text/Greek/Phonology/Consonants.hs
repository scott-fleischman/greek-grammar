{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Phonology.Consonants where

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


allConsonantPhonemes :: [ConsonantPhoneme]
allConsonantPhonemes = [Beta, Gamma, GammaNasal, Digamma, Delta, Zeta, Theta, ConsonantalIota, Kappa, Lambda,
  Mu, Nu, Xi, Pi, Rho, RhoRough, Sigma, Tau, Phi, Chi, Psi, RoughBreathing]

data ConsonantPhonemeClassification = ConsonantPhonemeClassification
  { vocalChords :: VocalChordsClassification
  , partOfMouth :: PartOfMouthClassification
  , airFlow :: AirFlowClassification
  }

data VocalChordsClassification = VocalChordsClassification
  { voiceless :: Cited [ConsonantPhoneme]
  , voiced :: Cited [ConsonantPhoneme]
  }

data PartOfMouthClassification = PartOfMouthClassification
  { labial :: Cited [ConsonantPhoneme]
  , lingual :: Cited [ConsonantPhoneme]
  , velar :: Cited [ConsonantPhoneme]
  , dental :: Cited [ConsonantPhoneme]
  , gutteral :: Cited [ConsonantPhoneme]
  }

data AirFlowClassification = AirFlowClassification
  { stop :: Cited [ConsonantPhoneme]
  , fricative :: Cited [ConsonantPhoneme]
  , affricate :: Cited [ConsonantPhoneme]
  , aspirate :: Cited [ConsonantPhoneme]
  , liquid :: Cited [ConsonantPhoneme]
  , nasal :: Cited [ConsonantPhoneme]
  , sibilant :: Cited [ConsonantPhoneme]
  }

mounceConsonants :: ConsonantPhonemeClassification
mounceConsonants = ConsonantPhonemeClassification mounceVocalChords mouncePartOfMouth mounceAirFlow

mounceVocalChords :: VocalChordsClassification
mounceVocalChords = VocalChordsClassification
  { voiceless = (++)
      <$> (mounce § "11.5" $ [Theta, Kappa, Xi, Pi, Sigma, Tau, Phi, Chi, Psi, RhoRough])
      <*> (mounce § "11.12" $ [RoughBreathing])
  , voiced = mounce § "11.6" $ [Beta, Gamma, Delta, Zeta, Lambda, Mu, Nu, Rho, ConsonantalIota, Digamma, GammaNasal]
  }

mouncePartOfMouth :: PartOfMouthClassification
mouncePartOfMouth = PartOfMouthClassification
  { labial = mounce § "11.9" $ [Pi, Beta, Phi, Mu, Psi, Digamma]
  , lingual = mounce § "11.9a" $ [Lambda, Rho, Nu]
  , velar = mounce § "11.10" $ [Kappa, Gamma, Chi, Xi]
  , dental = mounce § "11.11" $ [Tau, Delta, Theta, Zeta] -- When MBG refers to “dentals,” ζ is not included.
  , gutteral = mounce § "11.12" $ [RoughBreathing]
  }

mounceAirFlow :: AirFlowClassification
mounceAirFlow = AirFlowClassification
  { stop = mounce § "11.14" $ [Beta, Gamma, Delta, Pi, Kappa, Tau] -- The aspirates (φ χ θ) are sometimes called stops (Square of Stops)
  , fricative = mounce § "11.15" $ [GammaNasal, Lambda, Mu, Nu, Rho, Sigma, RoughBreathing] -- μ, ν sometimes called a liquid fricative
  , affricate = mounce § "11.16" $ [Psi, Xi, Zeta]
  , aspirate = mounce § "11.17" $ [Phi, Theta, Chi, RoughBreathing]
  , liquid = mounce § "11.18" $ [Lambda, Rho]
  , nasal = mounce § "11.19" $ [Mu, Nu, GammaNasal]
  , sibilant = mounce § "11.20" $ [Sigma, ConsonantalIota, Digamma] -- ζ ξ ψ The three double consonants are formed by adding δ (or ι), a velar, or a dental, respectively, to a sigma (§11. 16).
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
