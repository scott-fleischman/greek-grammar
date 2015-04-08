{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Phonology.Consonants where

import Text.Greek.Grammar

data Consonant

data VocalChords = Voiceless | Voiced

data PartOfMouth = Labial | Velar | Dental | Gutteral

data Fricative = Fricative

data AirFlow =
    Stop
  | Aspirate (Maybe Fricative) -- Stop + Aspiration
  | Liquid (Maybe Fricative)
  | DoubleConsonant
  | Nasal (Maybe Fricative)
  | Sibilant (Maybe Fricative)

data ConsonantPhoneme = Beta | Gamma | GammaNasal | Digamma | Delta | Zeta | Theta | ConsonantalIota | Kappa | Lambda
  | Mu | Nu | Xi | Pi | Rho | RhoRough | Sigma | Tau | Phi | Chi | Psi | RoughBreathing

-- GammaNasal : mounce "11.15" A gamma that is followed immediately by a velar (κ γ χ, including the velar double consonant ξ) is a gamma-nasal.
-- mounce "11.18" ρ Initial ρ always has rough breathing.

-- Vocal chords

voiceless :: Cited [ConsonantPhoneme]
voiceless = mounce §§ ["11.5", "11.12"] $ [Theta, Kappa, Xi, Pi, Sigma, Tau, Phi, Chi, Psi, RhoRough, RoughBreathing] -- ̔ The rough breathing is also a voiceless fricative

voiced :: Cited [ConsonantPhoneme]
voiced = mounce § "11.6" $ [Beta, Gamma, Delta, Zeta, Lambda, Mu, Nu, Rho, ConsonantalIota, Digamma, GammaNasal] -- β, γ, δ, ζ, λ, μ, ν, ρ (except ῥ), ι, ϝ, γ-nasal

-- Part of Mouth

labial :: Cited [ConsonantPhoneme]
labial = mounce § "11.9" $ [Pi, Beta, Phi, Mu, Psi, Digamma]

lingual :: Cited [ConsonantPhoneme]
lingual = mounce § "11.9a" $ [Lambda, Rho, Nu]

velar :: Cited [ConsonantPhoneme]
velar = mounce § "11.10" $ [Kappa, Gamma, Chi, Xi]

dental :: Cited [ConsonantPhoneme]
dental = mounce § "11.11" $ [Tau, Delta, Theta, Zeta]
-- When MBG refers to “dentals,” ζ is not included.

gutteral :: Cited [ConsonantPhoneme]
gutteral = mounce § "11.12" $ [RoughBreathing]


-- Air flow

stop :: Cited [ConsonantPhoneme]
stop = mounce § "11.14" $ [Beta, Gamma, Delta, Pi, Kappa, Tau]
-- The aspirates (φ χ θ) are sometimes called stops (Square of Stops)

fricative :: Cited [ConsonantPhoneme]
fricative = mounce § "11.15" $ [GammaNasal, Lambda, Mu, Nu, Rho, Sigma, RoughBreathing]
{-
μ nasal fricative (sometimes called a liquid fricative)
ν nasal fricative (sometimes called a liquid fricative)
-}

affricate :: Cited [ConsonantPhoneme]
affricate = mounce § "11.16" $ [Psi, Xi, Zeta]
{-
We will usually call affricates double consonants, since the term is more widely used.
ψ labial affricate (which can be formed through the combination of a labial [π β φ] and a sibilant)
ξ velar affricate (which can be formed through the combination of a velar [κ γ χ] and a sibilant)
ζ dental affricate (which is a combination of σδ, δσ, or δι).
-}

-- §11.16a  Assibilate. An assibilate is the combination of a stop and a sibilant. In Greek, all double consonants are assibilates
-- §11.16b. A π and a σ are pronounced exactly the same as a ψ. The change is merely orthographic, i.e., ψ is a way of writing πσ, βσ, or φσ

aspirate :: Cited [ConsonantPhoneme]
aspirate = mounce § "11.17" $ [Phi, Theta, Chi, RoughBreathing]
-- An aspirate is the combination of a stop and aspiration.

liquid :: Cited [ConsonantPhoneme]
liquid = mounce § "11.18" $ [Lambda, Rho]

nasal :: Cited [ConsonantPhoneme]
nasal = mounce § "11.19" $ [Mu, Nu, GammaNasal]

sibilant :: Cited [ConsonantPhoneme]
sibilant = mounce § "11.20" $ [Sigma, ConsonantalIota, Digamma]
{-
σ The sigma is the only true sibilant in Greek.
ζ ξ ψ The three double consonants are formed by adding δ (or ι), a velar, or a dental, respectively, to a sigma (§11. 16).
ι ϝ The semi-consonants also functioned as sibilants before they dropped out of use.
-}

-- §11.21 Sonant. A sonant is a consonant that produces a vowel sound either immediately before it or after it in order to aid pronunciation

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
