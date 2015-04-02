{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Phonology.Consonants where

import Data.Text (Text)
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

data Phoneme = Alpha | Beta | Gamma | GammaNasal | Digamma | Delta | Epsilon | Zeta | Eta | Theta | Iota | ConsonantalIota | Kappa | Lambda
  | Mu | Nu | Xi | Omicron | Pi | Rho | RhoRough | Sigma | Tau | Upsilon | Phi | Chi | Psi | Omega | RoughBreathing

type Cited a = (Citation, a)
mounce :: Text -> a -> Cited a
mounce t a = (mounceCitation t, a)


-- Vocal chords

voiceless :: Cited [Phoneme]
voiceless = mounce "11.5" [Theta, Kappa, Xi, Pi, Sigma, Tau, Phi, Chi, Psi, RhoRough] -- θ, κ, ξ, π, σ, τ, φ, χ, ψ, ῥ

voiced :: Cited [Phoneme]
voiced = mounce "11.6" [Beta, Gamma, Delta, Zeta, Lambda, Mu, Nu, Rho, ConsonantalIota, Digamma, GammaNasal] -- β, γ, δ, ζ, λ, μ, ν, ρ (except ῥ), ι, ϝ, γ-nasal


-- Part of Mouth

labial :: Cited [Phoneme]
labial = mounce "11.9" [Pi, Beta, Phi, Mu, Psi, Digamma]
{-
π voiceless labial
β voiced labial
φ labial aspirate
μ labial nasal (also called “labio-nasal”). BAGD refers to μ primarily as a nasal.
ψ labial affricate. MBG refers to ψ primarily as a double consonant.
ϝ labial fricative
-}

lingual :: Cited [Phoneme]
lingual = mounce "11.9a" [Lambda, Rho, Nu] -- λ ρ ν

velar :: Cited [Phoneme]
velar = mounce "11.10" [Kappa, Gamma, Chi, Xi]
{-
κ voiceless velar
γ voiced velar
χ velar aspirate
ξ velar affricate. MBG refers to ξ as a double consonant.
-}

dental :: Cited [Phoneme]
dental = mounce "11.11" [Tau, Delta, Theta, Zeta]
{-
τ voiceless dental
δ voiced dental
θ dental aspirate
ζ dental affricate. When MBG refers to “dentals,” ζ is not included.
-}

gutteral :: Cited [Phoneme]
gutteral = mounce "11.12" [RoughBreathing] -- ̔ The rough breathing is also a voiceless fricative


-- Air flow

stop :: Cited [Phoneme]
stop = mounce "11.14" [Beta, Gamma, Delta, Pi, Kappa, Tau]
{-
β, γ, δ (voiced)
π, κ, τ (voiceless)
The aspirates (φ χ θ) are sometimes called stops (Square of Stops)
-}

fricative :: Cited [Phoneme]
fricative = mounce "11.15" [GammaNasal, Lambda, Mu, Nu, Rho, Sigma, RoughBreathing]
{-
In the pronunciation of the consonant, the air flow is not stopped completely although its flow is impeded.
γγ  γ-nasal. A gamma that is followed immediately by a velar (κ γ χ, including the velar double consonant ξ) is a gamma-nasal. It is pronounced like a ν. Smyth gives the following examples that show how the pronunciation was carried over into Latin and English: ἄγκυρα (ancora; anchor), ἄγγελος (angelus; angel), σφίγξ (English: sphinx).
λ liquid fricative
μ nasal fricative (sometimes called a liquid fricative)
ν nasal fricative (sometimes called a liquid fricative)
ρ liquid fricative
σ sibilant fricative
̔ gutteral fricative.
-}

affricate :: Cited [Phoneme]
affricate = mounce "11.16" [Psi, Xi, Zeta]
{-
We will usually call affricates double consonants, since the term is more widely used.
ψ labial affricate (which can be formed through the combination of a labial [π β φ] and a sibilant)
ξ velar affricate (which can be formed through the combination of a velar [κ γ χ] and a sibilant)
ζ dental affricate (which is a combination of σδ, δσ, or δι).
-}

-- §11.16a  Assibilate. An assibilate is the combination of a stop and a sibilant. In Greek, all double consonants are assibilates
-- §11.16b. A π and a σ are pronounced exactly the same as a ψ. The change is merely orthographic, i.e., ψ is a way of writing πσ, βσ, or φσ

aspirate :: Cited [Phoneme]
aspirate = mounce "11.17" [Phi, Theta, Chi, RoughBreathing]
{-
An aspirate is the combination of a stop and aspiration.
φ labial aspirate
θ dental aspirate
χ velar aspirate
̔ rough breathing; cf. §11.15
-}

liquid :: Cited [Phoneme]
liquid = mounce "11.18" [Lambda, Rho]
{-
λ Also called “lateral.”
ρ Initial ρ always has rough breathing.
-}

nasal :: Cited [Phoneme]
nasal = mounce "11.19" [Mu, Nu, GammaNasal]
{-
μ labial nasal
ν dental nasal
γ-nasal velar nasal (formed with any velar preceded by γ.)
-}

sibilant :: Cited [Phoneme]
sibilant = mounce "11.20" [Sigma, ConsonantalIota, Digamma]
{-
σ The sigma is the only true sibilant in Greek.
ζ ξ ψ The three double consonants are formed by adding δ (or ι), a velar, or a dental, respectively, to a sigma (§11. 16).
ι ϝ The semi-consonants also functioned as sibilants before they dropped out of use.
-}

-- §11.21 Sonant. A sonant is a consonant that produces a vowel sound either immediately before it or after it in order to aid pronunciation
semivowel :: Cited [Phoneme]
semivowel = mounce "11.22" [ConsonantalIota, Digamma]

squareOfStops :: Cited [[Phoneme]]
squareOfStops = mounce "12.1"
--            Orders
-- Voiceless, Voiced, Aspirate
  [ [Pi,      Beta,   Phi]    -- Labial
  , [Kappa,   Gamma,  Chi]    -- Velar     Classes
  , [Tau,     Delta,  Theta]  -- Dental
  ]

table :: [(Phoneme, VocalChords, PartOfMouth, AirFlow)]
table =
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
