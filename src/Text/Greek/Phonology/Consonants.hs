module Text.Greek.Phonology.Consonants where

import Text.Greek.Script

data VocalChords = Voiceless | Voiced | Aspirate

data PartOfMouth = Labial | Velar | Dental | Gutteral

data AirFlow = Stop | Liquid | LiquidFricative | Affricate | DoubleConsonant | Nasal | NasalFricative | Sibilant 

data Misc = Sonant | Semivowel

mapping :: [(Letter, (VocalChords, PartOfMouth, AirFlow, Maybe Misc))]
mapping =
 [ (Beta, (Voiced, Labial, Stop, Nothing))
 , (Gamma, (Voiced, Velar, Stop, Nothing))
-- , (GammaNasal, (Voiced, Velar, NasalFricative, Nothing))
-- , (Digamma, (Voiced, Velar, Sibilant, Just Semivowel))
 , (Delta, (Voiced, Labial, Stop, Nothing))
 , (Zeta, (Voiced, Dental, DoubleConsonant, Nothing))
 , (Theta, (Aspirate, Dental, Stop, Nothing))
 , (Iota, (Voiced, Velar, Sibilant, Just Semivowel))
 , (Kappa, (Voiceless, Velar, Stop, Nothing))
 , (Lambda, (Voiced, Velar, LiquidFricative, Nothing))
 , (Mu, (Voiced, Labial, NasalFricative, Nothing))
 , (Nu, (Voiced, Dental, NasalFricative, Nothing))
 , (Xi, (Voiced, Velar, DoubleConsonant, Nothing))
 , (Pi, (Voiceless, Labial, Stop, Nothing))
 , (Rho, (Voiceless, Velar, LiquidFricative, Nothing))
 , (Rho, (Voiced, Velar, LiquidFricative, Nothing))
 , (Sigma, (Voiceless, Velar, Sibilant, Nothing))
-- , (FinalSigma, (Voicless, Velar, Spirant, Nothing))
 , (Tau, (Voiceless, Dental, Stop, Nothing))
 , (Phi, (Aspirate, Labial, Stop, Nothing))
 , (Chi, (Voiced, Velar, DoubleConsonant, Nothing))
 , (Psi, (Voiced, Labial, DoubleConsonant, Nothing))
-- , (RoughBreathing, (Voicless, Gutteral, Fricative, Nothing))
 ]
