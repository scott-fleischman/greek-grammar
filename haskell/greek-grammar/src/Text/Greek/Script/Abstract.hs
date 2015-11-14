{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Greek.Script.Abstract where

import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data Case = Lowercase | Uppercase deriving (Eq, Ord, Show)

data Final = FinalNotSupported | IsFinal | IsNotFinal deriving (Eq, Ord, Show)

newtype LetterIndex = LetterIndex { getLetterIndex :: Int } deriving (Eq, Show, Ord)
newtype LetterReverseIndex = LetterReverseIndex { getLetterReverseIndex :: Int } deriving (Eq, Show, Ord)
newtype CaseIndex = CaseIndex { getCaseIndex :: Int } deriving (Eq, Show, Ord)
newtype FinalReverseIndex = FinalReverseIndex { getFinalReverseIndex :: Int } deriving (Eq, Show, Ord)

toLetterCaseFinal :: Concrete.Letter -> (Letter, Case, Final)
toLetterCaseFinal Concrete.C_Α = (L_α, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Β = (L_β, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Γ = (L_γ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Δ = (L_δ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ε = (L_ε, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ζ = (L_ζ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Η = (L_η, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Θ = (L_θ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ι = (L_ι, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Κ = (L_κ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Λ = (L_λ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Μ = (L_μ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ν = (L_ν, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ξ = (L_ξ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ο = (L_ο, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Π = (L_π, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ρ = (L_ρ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Σ = (L_σ, Uppercase, IsNotFinal)
toLetterCaseFinal Concrete.C_Τ = (L_τ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Υ = (L_υ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Φ = (L_φ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Χ = (L_χ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ψ = (L_ψ, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_Ω = (L_ω, Uppercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_α = (L_α, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_β = (L_β, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_γ = (L_γ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_δ = (L_δ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ε = (L_ε, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ζ = (L_ζ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_η = (L_η, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_θ = (L_θ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ι = (L_ι, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_κ = (L_κ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_λ = (L_λ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_μ = (L_μ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ν = (L_ν, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ξ = (L_ξ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ο = (L_ο, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_π = (L_π, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ρ = (L_ρ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_σ = (L_σ, Lowercase, IsNotFinal)
toLetterCaseFinal Concrete.C_ς = (L_σ, Lowercase, IsFinal)
toLetterCaseFinal Concrete.C_τ = (L_τ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_υ = (L_υ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_φ = (L_φ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_χ = (L_χ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ψ = (L_ψ, Lowercase, FinalNotSupported)
toLetterCaseFinal Concrete.C_ω = (L_ω, Lowercase, FinalNotSupported)

letterToUnicode :: Letter -> Unicode.Letter
letterToUnicode L_α = Unicode.Letter 'α'
letterToUnicode L_β = Unicode.Letter 'β'
letterToUnicode L_γ = Unicode.Letter 'γ'
letterToUnicode L_δ = Unicode.Letter 'δ'
letterToUnicode L_ε = Unicode.Letter 'ε'
letterToUnicode L_ζ = Unicode.Letter 'ζ'
letterToUnicode L_η = Unicode.Letter 'η'
letterToUnicode L_θ = Unicode.Letter 'θ'
letterToUnicode L_ι = Unicode.Letter 'ι'
letterToUnicode L_κ = Unicode.Letter 'κ'
letterToUnicode L_λ = Unicode.Letter 'λ'
letterToUnicode L_μ = Unicode.Letter 'μ'
letterToUnicode L_ν = Unicode.Letter 'ν'
letterToUnicode L_ξ = Unicode.Letter 'ξ'
letterToUnicode L_ο = Unicode.Letter 'ο'
letterToUnicode L_π = Unicode.Letter 'π'
letterToUnicode L_ρ = Unicode.Letter 'ρ'
letterToUnicode L_σ = Unicode.Letter 'σ'
letterToUnicode L_τ = Unicode.Letter 'τ'
letterToUnicode L_υ = Unicode.Letter 'υ'
letterToUnicode L_φ = Unicode.Letter 'φ'
letterToUnicode L_χ = Unicode.Letter 'χ'
letterToUnicode L_ψ = Unicode.Letter 'ψ'
letterToUnicode L_ω = Unicode.Letter 'ω'


validateIsCapitalized :: (a -> Case) -> (a -> b) -> [a] -> Maybe (Word.IsCapitalized, [b])
validateIsCapitalized _ _ [] = Nothing
validateIsCapitalized f g xs@(x : xs') = case (f x, allLowercase xs') of
  (Lowercase, True) -> Just (Word.IsNotCapitalized, fmap g xs)
  (Uppercase, True) -> Just (Word.IsCapitalized, fmap g xs)
  _ -> Nothing
  where
    allLowercase = all ((== Lowercase) . f)

validateLetterFinal :: (a -> Final) -> (a -> b) -> [a] -> Maybe [b]
validateLetterFinal f g = fmap reverse . validateReverseList . reverse
  where
    validateReverseList [] = pure []
    validateReverseList (x : xs') = (:) <$> validateFinalLetter x <*> traverse validateNonFinalLetter xs'

    validateNonFinalLetter x | f x == FinalNotSupported || f x == IsNotFinal = Just $ g x
    validateNonFinalLetter _ = Nothing
    validateFinalLetter x | f x == FinalNotSupported || f x == IsFinal = Just $ g x
    validateFinalLetter _ = Nothing


data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω deriving (Eq, Show, Ord)
data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ deriving (Eq, Show, Ord)

type VowelConsonant = Either Vowel Consonant

toVowelConsonant :: Letter -> Either Vowel Consonant
toVowelConsonant L_α = Left V_α
toVowelConsonant L_ε = Left V_ε
toVowelConsonant L_η = Left V_η
toVowelConsonant L_ι = Left V_ι
toVowelConsonant L_ο = Left V_ο
toVowelConsonant L_υ = Left V_υ
toVowelConsonant L_ω = Left V_ω
toVowelConsonant L_β = Right C_β
toVowelConsonant L_γ = Right C_γ
toVowelConsonant L_δ = Right C_δ
toVowelConsonant L_ζ = Right C_ζ
toVowelConsonant L_θ = Right C_θ
toVowelConsonant L_κ = Right C_κ
toVowelConsonant L_λ = Right C_λ
toVowelConsonant L_μ = Right C_μ
toVowelConsonant L_ν = Right C_ν
toVowelConsonant L_ξ = Right C_ξ
toVowelConsonant L_π = Right C_π
toVowelConsonant L_ρ = Right C_ρ
toVowelConsonant L_σ = Right C_σ
toVowelConsonant L_τ = Right C_τ
toVowelConsonant L_φ = Right C_φ
toVowelConsonant L_χ = Right C_χ
toVowelConsonant L_ψ = Right C_ψ

vowelToLetter :: Vowel -> Letter
vowelToLetter V_α = L_α
vowelToLetter V_ε = L_ε
vowelToLetter V_η = L_η
vowelToLetter V_ι = L_ι
vowelToLetter V_ο = L_ο
vowelToLetter V_υ = L_υ
vowelToLetter V_ω = L_ω

consonantToLetter :: Consonant -> Letter
consonantToLetter C_β = L_β
consonantToLetter C_γ = L_γ
consonantToLetter C_δ = L_δ
consonantToLetter C_ζ = L_ζ
consonantToLetter C_θ = L_θ
consonantToLetter C_κ = L_κ
consonantToLetter C_λ = L_λ
consonantToLetter C_μ = L_μ
consonantToLetter C_ν = L_ν
consonantToLetter C_ξ = L_ξ
consonantToLetter C_π = L_π
consonantToLetter C_ρ = L_ρ
consonantToLetter C_σ = L_σ
consonantToLetter C_τ = L_τ
consonantToLetter C_φ = L_φ
consonantToLetter C_χ = L_χ
consonantToLetter C_ψ = L_ψ

type VowelCluster = [Vowel]
type ConsonantCluster = [Consonant]
type VowelConsonantCluster = Either VowelCluster ConsonantCluster
