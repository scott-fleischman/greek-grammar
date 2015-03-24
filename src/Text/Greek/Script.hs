{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script where

import Prelude (Bool(..), Eq, Show, not, ($), (==), (/=), (||), and, (&&))
import Control.Applicative ((<$>))
import Control.Lens (makeLenses)
import Data.List (elem)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequenceA)

data Letter =
    Alpha | Beta | Gamma | Delta | Epsilon | Zeta | Eta | Theta | Iota | Kappa | Lambda
  | Mu | Nu | Xi | Omicron | Pi | Rho | Sigma | Tau | Upsilon | Phi | Chi | Psi | Omega
  deriving (Eq, Show)
data LetterCase = Lowercase | Uppercase deriving (Eq, Show)
data Accent = Acute | Grave | Circumflex deriving (Eq, Show)
data Breathing = Smooth | Rough deriving (Eq, Show)
data IotaSubscript
data Diaeresis
data FinalForm

data Token = Token
  { _letter :: Letter
  , _letterCase :: LetterCase
  , _accent :: Maybe Accent
  , _breathing :: Maybe Breathing
  , _iotaSubscript :: Maybe IotaSubscript
  , _diaeresis :: Maybe Diaeresis
  , _finalForm :: Maybe FinalForm
  }
makeLenses ''Token

vowels :: [Letter]
vowels = [Alpha, Epsilon, Eta, Iota, Omicron, Upsilon, Omega]

isValidAccent :: Letter -> Accent -> Bool
isValidAccent el Acute = el `elem` vowels
isValidAccent el Grave = el `elem` vowels
isValidAccent el Circumflex = not $ el `elem` alwaysShortVowels
  where alwaysShortVowels = [Epsilon, Omicron]

isValidBreathing :: Letter -> LetterCase -> Breathing -> Bool
isValidBreathing el Lowercase Smooth = el `elem` vowels || el == Rho
isValidBreathing el Uppercase Smooth = el `elem` vowels && el /= Upsilon
isValidBreathing el _ Rough = el `elem` vowels || el == Rho

isValidIotaSubscript :: Letter -> IotaSubscript -> Bool
isValidIotaSubscript el _ = el `elem` iotaSubscriptVowels
  where iotaSubscriptVowels = [Alpha, Eta, Omega]

isValidDiaeresis :: Letter -> Diaeresis -> Bool
isValidDiaeresis el _ = el `elem` diaeresisVowels
  where diaeresisVowels = [Iota, Upsilon]

isValidFinalForm :: Letter -> LetterCase -> FinalForm -> Bool
isValidFinalForm Sigma Lowercase _ = True
isValidFinalForm _ _ _ = False

isValidToken :: Token -> Bool
isValidToken (Token el c a b is d f) =
  case (sequenceA validations) of
    Just rs -> and rs
    _ -> True
  where
    validations :: [Maybe Bool]
    validations =
      [ isValidAccent el <$> a
      , isValidBreathing el c <$> b
      , isValidIotaSubscript el <$> is
      , isValidDiaeresis el <$> d
      , isValidFinalForm el c <$> f
      ]
