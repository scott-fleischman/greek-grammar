module Text.Greek.Script.Concrete where

--import qualified Text.Greek.Script.Unit as Unit
import qualified Text.Greek.Script.Unicode as Unicode

data Letter
  = C_Α | C_Β | C_Γ | C_Δ | C_Ε | C_Ζ | C_Η | C_Θ | C_Ι | C_Κ | C_Λ | C_Μ | C_Ν | C_Ξ | C_Ο | C_Π | C_Ρ | C_Σ       | C_Τ | C_Υ | C_Φ | C_Χ | C_Ψ | C_Ω
  | C_α | C_β | C_γ | C_δ | C_ε | C_ζ | C_η | C_θ | C_ι | C_κ | C_λ | C_μ | C_ν | C_ξ | C_ο | C_π | C_ρ | C_σ | C_ς | C_τ | C_υ | C_φ | C_χ | C_ψ | C_ω
  deriving (Eq, Ord, Show)

data Mark = Acute | Grave | Circumflex | Smooth | Rough | IotaSubscript | Diaeresis
  deriving (Eq, Ord, Show)

--data Error
--  = ErrorLetter FileCharReference Unicode.Letter
--  | ErrorMark FileCharReference Unicode.Mark
--  deriving (Show)

--type Unit = Unit.UnitMarkList Letter Mark

--toUnit :: Unit.UnitChar -> Either Error Unit
--toUnit x = U.unitItem toLetter x >>= (U.unitMarks . traverse) toMark

--toLetter :: (Unicode.Letter, FileCharReference) -> Either Error (Letter, FileCharReference)
--toLetter (l, r) | Just l' <- toMaybeLetter l = Right (l', r)
--toLetter (l, r) = Left $ ErrorLetter r l

--toMark :: (Unicode.Mark, FileCharReference) -> Either Error (Mark, FileCharReference)
--toMark (m, r) | Just m' <- toMaybeMark m = Right (m', r)
--toMark (m, r) = Left $ ErrorMark r m

toMaybeLetter :: Unicode.Letter -> Maybe Letter
toMaybeLetter (Unicode.Letter 'Α') = Just C_Α
toMaybeLetter (Unicode.Letter 'Β') = Just C_Β
toMaybeLetter (Unicode.Letter 'Γ') = Just C_Γ
toMaybeLetter (Unicode.Letter 'Δ') = Just C_Δ
toMaybeLetter (Unicode.Letter 'Ε') = Just C_Ε
toMaybeLetter (Unicode.Letter 'Ζ') = Just C_Ζ
toMaybeLetter (Unicode.Letter 'Η') = Just C_Η
toMaybeLetter (Unicode.Letter 'Θ') = Just C_Θ
toMaybeLetter (Unicode.Letter 'Ι') = Just C_Ι
toMaybeLetter (Unicode.Letter 'Κ') = Just C_Κ
toMaybeLetter (Unicode.Letter 'Λ') = Just C_Λ
toMaybeLetter (Unicode.Letter 'Μ') = Just C_Μ
toMaybeLetter (Unicode.Letter 'Ν') = Just C_Ν
toMaybeLetter (Unicode.Letter 'Ξ') = Just C_Ξ
toMaybeLetter (Unicode.Letter 'Ο') = Just C_Ο
toMaybeLetter (Unicode.Letter 'Π') = Just C_Π
toMaybeLetter (Unicode.Letter 'Ρ') = Just C_Ρ
toMaybeLetter (Unicode.Letter 'Σ') = Just C_Σ
toMaybeLetter (Unicode.Letter 'Τ') = Just C_Τ
toMaybeLetter (Unicode.Letter 'Υ') = Just C_Υ
toMaybeLetter (Unicode.Letter 'Φ') = Just C_Φ
toMaybeLetter (Unicode.Letter 'Χ') = Just C_Χ
toMaybeLetter (Unicode.Letter 'Ψ') = Just C_Ψ
toMaybeLetter (Unicode.Letter 'Ω') = Just C_Ω
toMaybeLetter (Unicode.Letter 'α') = Just C_α
toMaybeLetter (Unicode.Letter 'β') = Just C_β
toMaybeLetter (Unicode.Letter 'γ') = Just C_γ
toMaybeLetter (Unicode.Letter 'δ') = Just C_δ
toMaybeLetter (Unicode.Letter 'ε') = Just C_ε
toMaybeLetter (Unicode.Letter 'ζ') = Just C_ζ
toMaybeLetter (Unicode.Letter 'η') = Just C_η
toMaybeLetter (Unicode.Letter 'θ') = Just C_θ
toMaybeLetter (Unicode.Letter 'ι') = Just C_ι
toMaybeLetter (Unicode.Letter 'κ') = Just C_κ
toMaybeLetter (Unicode.Letter 'λ') = Just C_λ
toMaybeLetter (Unicode.Letter 'μ') = Just C_μ
toMaybeLetter (Unicode.Letter 'ν') = Just C_ν
toMaybeLetter (Unicode.Letter 'ξ') = Just C_ξ
toMaybeLetter (Unicode.Letter 'ο') = Just C_ο
toMaybeLetter (Unicode.Letter 'π') = Just C_π
toMaybeLetter (Unicode.Letter 'ρ') = Just C_ρ
toMaybeLetter (Unicode.Letter 'ς') = Just C_ς
toMaybeLetter (Unicode.Letter 'σ') = Just C_σ
toMaybeLetter (Unicode.Letter 'τ') = Just C_τ
toMaybeLetter (Unicode.Letter 'υ') = Just C_υ
toMaybeLetter (Unicode.Letter 'φ') = Just C_φ
toMaybeLetter (Unicode.Letter 'χ') = Just C_χ
toMaybeLetter (Unicode.Letter 'ψ') = Just C_ψ
toMaybeLetter (Unicode.Letter 'ω') = Just C_ω
toMaybeLetter _ = Nothing

letterToUnicode :: Letter -> Unicode.Letter
letterToUnicode C_Α = (Unicode.Letter 'Α')
letterToUnicode C_Β = (Unicode.Letter 'Β')
letterToUnicode C_Γ = (Unicode.Letter 'Γ')
letterToUnicode C_Δ = (Unicode.Letter 'Δ')
letterToUnicode C_Ε = (Unicode.Letter 'Ε')
letterToUnicode C_Ζ = (Unicode.Letter 'Ζ')
letterToUnicode C_Η = (Unicode.Letter 'Η')
letterToUnicode C_Θ = (Unicode.Letter 'Θ')
letterToUnicode C_Ι = (Unicode.Letter 'Ι')
letterToUnicode C_Κ = (Unicode.Letter 'Κ')
letterToUnicode C_Λ = (Unicode.Letter 'Λ')
letterToUnicode C_Μ = (Unicode.Letter 'Μ')
letterToUnicode C_Ν = (Unicode.Letter 'Ν')
letterToUnicode C_Ξ = (Unicode.Letter 'Ξ')
letterToUnicode C_Ο = (Unicode.Letter 'Ο')
letterToUnicode C_Π = (Unicode.Letter 'Π')
letterToUnicode C_Ρ = (Unicode.Letter 'Ρ')
letterToUnicode C_Σ = (Unicode.Letter 'Σ')
letterToUnicode C_Τ = (Unicode.Letter 'Τ')
letterToUnicode C_Υ = (Unicode.Letter 'Υ')
letterToUnicode C_Φ = (Unicode.Letter 'Φ')
letterToUnicode C_Χ = (Unicode.Letter 'Χ')
letterToUnicode C_Ψ = (Unicode.Letter 'Ψ')
letterToUnicode C_Ω = (Unicode.Letter 'Ω')
letterToUnicode C_α = (Unicode.Letter 'α')
letterToUnicode C_β = (Unicode.Letter 'β')
letterToUnicode C_γ = (Unicode.Letter 'γ')
letterToUnicode C_δ = (Unicode.Letter 'δ')
letterToUnicode C_ε = (Unicode.Letter 'ε')
letterToUnicode C_ζ = (Unicode.Letter 'ζ')
letterToUnicode C_η = (Unicode.Letter 'η')
letterToUnicode C_θ = (Unicode.Letter 'θ')
letterToUnicode C_ι = (Unicode.Letter 'ι')
letterToUnicode C_κ = (Unicode.Letter 'κ')
letterToUnicode C_λ = (Unicode.Letter 'λ')
letterToUnicode C_μ = (Unicode.Letter 'μ')
letterToUnicode C_ν = (Unicode.Letter 'ν')
letterToUnicode C_ξ = (Unicode.Letter 'ξ')
letterToUnicode C_ο = (Unicode.Letter 'ο')
letterToUnicode C_π = (Unicode.Letter 'π')
letterToUnicode C_ρ = (Unicode.Letter 'ρ')
letterToUnicode C_ς = (Unicode.Letter 'ς')
letterToUnicode C_σ = (Unicode.Letter 'σ')
letterToUnicode C_τ = (Unicode.Letter 'τ')
letterToUnicode C_υ = (Unicode.Letter 'υ')
letterToUnicode C_φ = (Unicode.Letter 'φ')
letterToUnicode C_χ = (Unicode.Letter 'χ')
letterToUnicode C_ψ = (Unicode.Letter 'ψ')
letterToUnicode C_ω = (Unicode.Letter 'ω')

toMaybeMark :: Unicode.Mark -> Maybe Mark
toMaybeMark (Unicode.Mark '\x0300') = Just Grave          -- COMBINING GRAVE ACCENT
toMaybeMark (Unicode.Mark '\x0301') = Just Acute          -- COMBINING ACUTE ACCENT
toMaybeMark (Unicode.Mark '\x0308') = Just Diaeresis      -- COMBINING DIAERESIS
toMaybeMark (Unicode.Mark '\x0313') = Just Smooth         -- COMBINING COMMA ABOVE
toMaybeMark (Unicode.Mark '\x0314') = Just Rough          -- COMBINING REVERSED COMMA ABOVE
toMaybeMark (Unicode.Mark '\x0342') = Just Circumflex     -- COMBINING GREEK PERISPOMENI
toMaybeMark (Unicode.Mark '\x0345') = Just IotaSubscript  -- COMBINING GREEK YPOGEGRAMMENI
toMaybeMark _ = Nothing

markToUnicode :: Mark -> Unicode.Mark
markToUnicode Grave         = Unicode.Mark '\x0300' -- COMBINING GRAVE ACCENT
markToUnicode Acute         = Unicode.Mark '\x0301' -- COMBINING ACUTE ACCENT
markToUnicode Diaeresis     = Unicode.Mark '\x0308' -- COMBINING DIAERESIS
markToUnicode Smooth        = Unicode.Mark '\x0313' -- COMBINING COMMA ABOVE
markToUnicode Rough         = Unicode.Mark '\x0314' -- COMBINING REVERSED COMMA ABOVE
markToUnicode Circumflex    = Unicode.Mark '\x0342' -- COMBINING GREEK PERISPOMENI
markToUnicode IotaSubscript = Unicode.Mark '\x0345' -- COMBINING GREEK YPOGEGRAMMENI
