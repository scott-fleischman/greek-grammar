{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Token where

import Prelude (Bool(..), Eq(..), Show(..), Ord, not, ($), (.), (||), (&&), fmap, snd)
import Control.Lens (makeLenses, (^.))
import Data.Foldable (concatMap)
import Data.List (elem, nub)
import Data.Maybe (Maybe(..), maybeToList)
import Text.Greek.Grammar

data Letter =
    Alpha | Beta | Gamma | Delta | Epsilon | Digamma | Zeta | Eta | Theta | Iota | Kappa | Lambda
  | Mu | Nu | Xi | Omicron | Pi | Rho | Sigma | Tau | Upsilon | Phi | Chi | Psi | Omega
  deriving (Eq, Show, Ord)
data LetterCase = Lowercase | Uppercase deriving (Eq, Show, Ord)
data Accent = Acute | Grave | Circumflex deriving (Eq, Show, Ord)
data Breathing = Smooth | Rough deriving (Eq, Show, Ord)
data LengthMark = Breve | Macron deriving (Eq, Show, Ord)
data IotaSubscript = IotaSubscript deriving (Eq, Show, Ord)
data Diaeresis = Diaeresis deriving (Eq, Show, Ord)
data FinalForm = FinalForm deriving (Eq, Show, Ord)
data SemivowelMark = SemivowelMark deriving (Eq, Show, Ord) -- Smyth 20 U+032F ι̯
data SonantMark = SonantMark deriving (Eq, Show, Ord) -- Smyth 20 U+0325 λ̥

data Token = Token
  { _letter :: Letter
  , _letterCase :: LetterCase
  , _accent :: Maybe Accent
  , _breathing :: Maybe Breathing
  , _lengthMark :: Maybe LengthMark
  , _iotaSubscript :: Maybe IotaSubscript
  , _diaeresis :: Maybe Diaeresis
  , _finalForm :: Maybe FinalForm
  , _semivowelMark :: Maybe SemivowelMark
  , _sonantMark :: Maybe SonantMark
  }
  deriving (Show)
makeLenses ''Token

data TokenContext a = TokenContext
  { _token :: Token
  , _context :: a
  }
makeLenses ''TokenContext

instance Show (TokenContext a) where
  show t = show (t ^. token)

unmarkedLetter :: Letter -> LetterCase -> Token
unmarkedLetter el c = Token el c Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

vowels :: [Letter]
vowels = [Alpha, Epsilon, Eta, Iota, Omicron, Upsilon, Omega]

iotaSubscriptVowels :: [Letter]
iotaSubscriptVowels = [Alpha, Eta, Omega]

alwaysShortVowels :: [Letter]
alwaysShortVowels = [Epsilon, Omicron]

alwaysLongVowels :: [Letter]
alwaysLongVowels = [Eta, Omega]

sonantLetters :: Cited [Letter]
sonantLetters = smyth § "20 b" $ [Lambda, Mu, Nu, Rho, Sigma]

diphthongs :: [(Letter, Letter)]
diphthongs =
  [ (Alpha, Iota)
  , (Epsilon, Iota)
  , (Omicron, Iota)
  , (Upsilon, Iota)
  , (Alpha, Upsilon)
  , (Epsilon, Upsilon)
  , (Omicron, Upsilon)
  , (Eta, Upsilon)
  ]

diphthongSecondVowels :: [Letter]
diphthongSecondVowels = nub . fmap snd $ diphthongs

isValidAccent :: Letter -> Accent -> Bool
isValidAccent el Acute = el `elem` vowels
isValidAccent el Grave = el `elem` vowels
isValidAccent el Circumflex = el `elem` vowels && not (el `elem` alwaysShortVowels)

isValidBreathing :: Letter -> LetterCase -> Breathing -> Bool
isValidBreathing el Lowercase Smooth = el `elem` vowels || el == Rho
isValidBreathing el Uppercase Smooth = el `elem` vowels && el /= Upsilon
isValidBreathing el _ Rough = el `elem` vowels || el == Rho

isValidLengthMark :: Letter -> LengthMark -> Bool
isValidLengthMark el _ = not (el `elem` alwaysShortVowels) && not (el `elem` alwaysLongVowels)

isValidIotaSubscript :: Letter -> IotaSubscript -> Bool
isValidIotaSubscript el _ = el `elem` iotaSubscriptVowels

isValidDiaeresis :: Letter -> Diaeresis -> Bool
isValidDiaeresis el _ = el `elem` diphthongSecondVowels

isValidFinalForm :: Letter -> LetterCase -> FinalForm -> Bool
isValidFinalForm Sigma Lowercase _ = True
isValidFinalForm _ _ _ = False

isValidSemivowelMark :: Letter -> SemivowelMark -> Bool
isValidSemivowelMark el _ = el `elem` diphthongSecondVowels

isValidSonantMark :: Letter -> SonantMark -> Bool
isValidSonantMark el _ = el `elem` (sonantLetters ^. item)

data ValidationError =
    AccentError | BreathingError | LengthMarkError
  | IotaSubscriptError | DiaeresisError | FinalFormError | SemivowelMarkError | SonantMarkError
  deriving (Eq, Show)

validateItem :: (a -> Bool) -> Maybe a -> ValidationError -> Maybe ValidationError
validateItem _ Nothing _ = Nothing
validateItem isValid (Just v) e = case isValid v of
  True -> Nothing
  False -> Just e

validateToken :: Token -> [ValidationError]
validateToken (Token el c a b lm is d f sv sn) = concatMap maybeToList $
  [ validateItem (isValidAccent el) a AccentError
  , validateItem (isValidBreathing el c) b BreathingError
  , validateItem (isValidLengthMark el) lm LengthMarkError
  , validateItem (isValidIotaSubscript el) is IotaSubscriptError
  , validateItem (isValidDiaeresis el) d DiaeresisError
  , validateItem (isValidFinalForm el c) f FinalFormError
  , validateItem (isValidSemivowelMark el) sv SemivowelMarkError
  , validateItem (isValidSonantMark el) sn SonantMarkError
  ]
