module Text.Greek.Script.Concrete where

import Text.Greek.FileReference
import qualified Text.Greek.Script.Unit as Unit
import qualified Text.Greek.Script.Unicode as Unicode

data Letter
  = C_Α | C_Β | C_Γ | C_Δ | C_Ε | C_Ζ | C_Η | C_Θ | C_Ι | C_Κ | C_Λ | C_Μ | C_Ν | C_Ξ | C_Ο | C_Π | C_Ρ | C_Σ       | C_Τ | C_Υ | C_Φ | C_Χ | C_Ψ | C_Ω
  | C_α | C_β | C_γ | C_δ | C_ε | C_ζ | C_η | C_θ | C_ι | C_κ | C_λ | C_μ | C_ν | C_ξ | C_ο | C_π | C_ρ | C_σ | C_ς | C_τ | C_υ | C_φ | C_χ | C_ψ | C_ω
  deriving (Eq, Ord, Show)

data Mark = Acute | Grave | Circumflex | Smooth | Rough | IotaSubscript | Diaeresis
  deriving (Eq, Ord, Show)

data Error
  = ErrorLetter FileCharReference Unicode.Letter
  | ErrorMark FileCharReference Unicode.Mark
  deriving (Show)

type Unit = Unit.UnitMarkList Letter Mark

toUnit :: Unit.UnitChar -> Either Error Unit
toUnit x = U.unitItem toLetter x >>= (U.unitMarks . traverse) toMark

toLetter :: (Unicode.Letter, FileCharReference) -> Either Error (Letter, FileCharReference)
toLetter (l, r) | Just l' <- toMaybeLetter l = Right (l', r)
toLetter (l, r) = Left $ ErrorLetter r l

toMark :: (Unicode.Mark, FileCharReference) -> Either Error (Mark, FileCharReference)
toMark (m, r) | Just m' <- toMaybeMark m = Right (m', r)
toMark (m, r) = Left $ ErrorMark r m

toMaybeLetter :: U.LetterChar -> Maybe Letter
toMaybeLetter (U.LetterChar 'Α') = Just C_Α
toMaybeLetter (U.LetterChar 'Β') = Just C_Β
toMaybeLetter (U.LetterChar 'Γ') = Just C_Γ
toMaybeLetter (U.LetterChar 'Δ') = Just C_Δ
toMaybeLetter (U.LetterChar 'Ε') = Just C_Ε
toMaybeLetter (U.LetterChar 'Ζ') = Just C_Ζ
toMaybeLetter (U.LetterChar 'Η') = Just C_Η
toMaybeLetter (U.LetterChar 'Θ') = Just C_Θ
toMaybeLetter (U.LetterChar 'Ι') = Just C_Ι
toMaybeLetter (U.LetterChar 'Κ') = Just C_Κ
toMaybeLetter (U.LetterChar 'Λ') = Just C_Λ
toMaybeLetter (U.LetterChar 'Μ') = Just C_Μ
toMaybeLetter (U.LetterChar 'Ν') = Just C_Ν
toMaybeLetter (U.LetterChar 'Ξ') = Just C_Ξ
toMaybeLetter (U.LetterChar 'Ο') = Just C_Ο
toMaybeLetter (U.LetterChar 'Π') = Just C_Π
toMaybeLetter (U.LetterChar 'Ρ') = Just C_Ρ
toMaybeLetter (U.LetterChar 'Σ') = Just C_Σ
toMaybeLetter (U.LetterChar 'Τ') = Just C_Τ
toMaybeLetter (U.LetterChar 'Υ') = Just C_Υ
toMaybeLetter (U.LetterChar 'Φ') = Just C_Φ
toMaybeLetter (U.LetterChar 'Χ') = Just C_Χ
toMaybeLetter (U.LetterChar 'Ψ') = Just C_Ψ
toMaybeLetter (U.LetterChar 'Ω') = Just C_Ω
toMaybeLetter (U.LetterChar 'α') = Just C_α
toMaybeLetter (U.LetterChar 'β') = Just C_β
toMaybeLetter (U.LetterChar 'γ') = Just C_γ
toMaybeLetter (U.LetterChar 'δ') = Just C_δ
toMaybeLetter (U.LetterChar 'ε') = Just C_ε
toMaybeLetter (U.LetterChar 'ζ') = Just C_ζ
toMaybeLetter (U.LetterChar 'η') = Just C_η
toMaybeLetter (U.LetterChar 'θ') = Just C_θ
toMaybeLetter (U.LetterChar 'ι') = Just C_ι
toMaybeLetter (U.LetterChar 'κ') = Just C_κ
toMaybeLetter (U.LetterChar 'λ') = Just C_λ
toMaybeLetter (U.LetterChar 'μ') = Just C_μ
toMaybeLetter (U.LetterChar 'ν') = Just C_ν
toMaybeLetter (U.LetterChar 'ξ') = Just C_ξ
toMaybeLetter (U.LetterChar 'ο') = Just C_ο
toMaybeLetter (U.LetterChar 'π') = Just C_π
toMaybeLetter (U.LetterChar 'ρ') = Just C_ρ
toMaybeLetter (U.LetterChar 'ς') = Just C_ς
toMaybeLetter (U.LetterChar 'σ') = Just C_σ
toMaybeLetter (U.LetterChar 'τ') = Just C_τ
toMaybeLetter (U.LetterChar 'υ') = Just C_υ
toMaybeLetter (U.LetterChar 'φ') = Just C_φ
toMaybeLetter (U.LetterChar 'χ') = Just C_χ
toMaybeLetter (U.LetterChar 'ψ') = Just C_ψ
toMaybeLetter (U.LetterChar 'ω') = Just C_ω
toMaybeLetter _ = Nothing

letterToLetterChar :: Letter -> U.LetterChar
letterToLetterChar C_Α = (U.LetterChar 'Α')
letterToLetterChar C_Β = (U.LetterChar 'Β')
letterToLetterChar C_Γ = (U.LetterChar 'Γ')
letterToLetterChar C_Δ = (U.LetterChar 'Δ')
letterToLetterChar C_Ε = (U.LetterChar 'Ε')
letterToLetterChar C_Ζ = (U.LetterChar 'Ζ')
letterToLetterChar C_Η = (U.LetterChar 'Η')
letterToLetterChar C_Θ = (U.LetterChar 'Θ')
letterToLetterChar C_Ι = (U.LetterChar 'Ι')
letterToLetterChar C_Κ = (U.LetterChar 'Κ')
letterToLetterChar C_Λ = (U.LetterChar 'Λ')
letterToLetterChar C_Μ = (U.LetterChar 'Μ')
letterToLetterChar C_Ν = (U.LetterChar 'Ν')
letterToLetterChar C_Ξ = (U.LetterChar 'Ξ')
letterToLetterChar C_Ο = (U.LetterChar 'Ο')
letterToLetterChar C_Π = (U.LetterChar 'Π')
letterToLetterChar C_Ρ = (U.LetterChar 'Ρ')
letterToLetterChar C_Σ = (U.LetterChar 'Σ')
letterToLetterChar C_Τ = (U.LetterChar 'Τ')
letterToLetterChar C_Υ = (U.LetterChar 'Υ')
letterToLetterChar C_Φ = (U.LetterChar 'Φ')
letterToLetterChar C_Χ = (U.LetterChar 'Χ')
letterToLetterChar C_Ψ = (U.LetterChar 'Ψ')
letterToLetterChar C_Ω = (U.LetterChar 'Ω')
letterToLetterChar C_α = (U.LetterChar 'α')
letterToLetterChar C_β = (U.LetterChar 'β')
letterToLetterChar C_γ = (U.LetterChar 'γ')
letterToLetterChar C_δ = (U.LetterChar 'δ')
letterToLetterChar C_ε = (U.LetterChar 'ε')
letterToLetterChar C_ζ = (U.LetterChar 'ζ')
letterToLetterChar C_η = (U.LetterChar 'η')
letterToLetterChar C_θ = (U.LetterChar 'θ')
letterToLetterChar C_ι = (U.LetterChar 'ι')
letterToLetterChar C_κ = (U.LetterChar 'κ')
letterToLetterChar C_λ = (U.LetterChar 'λ')
letterToLetterChar C_μ = (U.LetterChar 'μ')
letterToLetterChar C_ν = (U.LetterChar 'ν')
letterToLetterChar C_ξ = (U.LetterChar 'ξ')
letterToLetterChar C_ο = (U.LetterChar 'ο')
letterToLetterChar C_π = (U.LetterChar 'π')
letterToLetterChar C_ρ = (U.LetterChar 'ρ')
letterToLetterChar C_ς = (U.LetterChar 'ς')
letterToLetterChar C_σ = (U.LetterChar 'σ')
letterToLetterChar C_τ = (U.LetterChar 'τ')
letterToLetterChar C_υ = (U.LetterChar 'υ')
letterToLetterChar C_φ = (U.LetterChar 'φ')
letterToLetterChar C_χ = (U.LetterChar 'χ')
letterToLetterChar C_ψ = (U.LetterChar 'ψ')
letterToLetterChar C_ω = (U.LetterChar 'ω')

toMaybeMark :: U.MarkChar -> Maybe Mark
toMaybeMark (U.MarkChar '\x0300') = Just Grave          -- COMBINING GRAVE ACCENT
toMaybeMark (U.MarkChar '\x0301') = Just Acute          -- COMBINING ACUTE ACCENT
toMaybeMark (U.MarkChar '\x0308') = Just Diaeresis      -- COMBINING DIAERESIS
toMaybeMark (U.MarkChar '\x0313') = Just Smooth         -- COMBINING COMMA ABOVE
toMaybeMark (U.MarkChar '\x0314') = Just Rough          -- COMBINING REVERSED COMMA ABOVE
toMaybeMark (U.MarkChar '\x0342') = Just Circumflex     -- COMBINING GREEK PERISPOMENI
toMaybeMark (U.MarkChar '\x0345') = Just IotaSubscript  -- COMBINING GREEK YPOGEGRAMMENI
toMaybeMark _ = Nothing

markToMarkChar :: Mark -> U.MarkChar
markToMarkChar Grave         = U.MarkChar '\x0300' -- COMBINING GRAVE ACCENT
markToMarkChar Acute         = U.MarkChar '\x0301' -- COMBINING ACUTE ACCENT
markToMarkChar Diaeresis     = U.MarkChar '\x0308' -- COMBINING DIAERESIS
markToMarkChar Smooth        = U.MarkChar '\x0313' -- COMBINING COMMA ABOVE
markToMarkChar Rough         = U.MarkChar '\x0314' -- COMBINING REVERSED COMMA ABOVE
markToMarkChar Circumflex    = U.MarkChar '\x0342' -- COMBINING GREEK PERISPOMENI
markToMarkChar IotaSubscript = U.MarkChar '\x0345' -- COMBINING GREEK YPOGEGRAMMENI
