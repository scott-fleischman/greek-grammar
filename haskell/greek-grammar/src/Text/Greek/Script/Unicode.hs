module Text.Greek.Script.Unicode where

import Text.Greek.FileReference
import qualified Text.Greek.Script.Unit as U

data UnicodeLetter
  = U_Α | U_Β | U_Γ | U_Δ | U_Ε | U_Ζ | U_Η | U_Θ | U_Ι | U_Κ | U_Λ | U_Μ | U_Ν | U_Ξ | U_Ο | U_Π | U_Ρ | U_Σ       | U_Τ | U_Υ | U_Φ | U_Χ | U_Ψ | U_Ω
  | U_α | U_β | U_γ | U_δ | U_ε | U_ζ | U_η | U_θ | U_ι | U_κ | U_λ | U_μ | U_ν | U_ξ | U_ο | U_π | U_ρ | U_σ | U_ς | U_τ | U_υ | U_φ | U_χ | U_ψ | U_ω
  deriving (Eq, Ord, Show)

data UnicodeMark = AcuteMark | GraveMark | CircumflexMark | SmoothMark | RoughMark | IotaSubscriptMark | DiaeresisMark
  deriving (Eq, Ord, Show)

data UnicodeError
  = UnicodeErrorLetter FileCharReference U.LetterChar
  | UnicodeErrorMark FileCharReference U.MarkChar

toUnicodeLetter :: (U.LetterChar, FileCharReference) -> Either UnicodeError (UnicodeLetter, FileCharReference)
toUnicodeLetter (l, r) | Just l' <- toMaybeUnicodeLetter l = Right (l', r)
toUnicodeLetter (l, r) = Left $ UnicodeErrorLetter r l

toUnicodeMark :: (U.MarkChar, FileCharReference) -> Either UnicodeError (UnicodeMark, FileCharReference)
toUnicodeMark (m, r) | Just m' <- toMaybeUnicodeMark m = Right (m', r)
toUnicodeMark (m, r) = Left $ UnicodeErrorMark r m

toMaybeUnicodeLetter :: U.LetterChar -> Maybe UnicodeLetter
toMaybeUnicodeLetter (U.LetterChar 'Α') = Just U_Α
toMaybeUnicodeLetter (U.LetterChar 'Β') = Just U_Β
toMaybeUnicodeLetter (U.LetterChar 'Γ') = Just U_Γ
toMaybeUnicodeLetter (U.LetterChar 'Δ') = Just U_Δ
toMaybeUnicodeLetter (U.LetterChar 'Ε') = Just U_Ε
toMaybeUnicodeLetter (U.LetterChar 'Ζ') = Just U_Ζ
toMaybeUnicodeLetter (U.LetterChar 'Η') = Just U_Η
toMaybeUnicodeLetter (U.LetterChar 'Θ') = Just U_Θ
toMaybeUnicodeLetter (U.LetterChar 'Ι') = Just U_Ι
toMaybeUnicodeLetter (U.LetterChar 'Κ') = Just U_Κ
toMaybeUnicodeLetter (U.LetterChar 'Λ') = Just U_Λ
toMaybeUnicodeLetter (U.LetterChar 'Μ') = Just U_Μ
toMaybeUnicodeLetter (U.LetterChar 'Ν') = Just U_Ν
toMaybeUnicodeLetter (U.LetterChar 'Ξ') = Just U_Ξ
toMaybeUnicodeLetter (U.LetterChar 'Ο') = Just U_Ο
toMaybeUnicodeLetter (U.LetterChar 'Π') = Just U_Π
toMaybeUnicodeLetter (U.LetterChar 'Ρ') = Just U_Ρ
toMaybeUnicodeLetter (U.LetterChar 'Σ') = Just U_Σ
toMaybeUnicodeLetter (U.LetterChar 'Τ') = Just U_Τ
toMaybeUnicodeLetter (U.LetterChar 'Υ') = Just U_Υ
toMaybeUnicodeLetter (U.LetterChar 'Φ') = Just U_Φ
toMaybeUnicodeLetter (U.LetterChar 'Χ') = Just U_Χ
toMaybeUnicodeLetter (U.LetterChar 'Ψ') = Just U_Ψ
toMaybeUnicodeLetter (U.LetterChar 'Ω') = Just U_Ω
toMaybeUnicodeLetter (U.LetterChar 'α') = Just U_α
toMaybeUnicodeLetter (U.LetterChar 'β') = Just U_β
toMaybeUnicodeLetter (U.LetterChar 'γ') = Just U_γ
toMaybeUnicodeLetter (U.LetterChar 'δ') = Just U_δ
toMaybeUnicodeLetter (U.LetterChar 'ε') = Just U_ε
toMaybeUnicodeLetter (U.LetterChar 'ζ') = Just U_ζ
toMaybeUnicodeLetter (U.LetterChar 'η') = Just U_η
toMaybeUnicodeLetter (U.LetterChar 'θ') = Just U_θ
toMaybeUnicodeLetter (U.LetterChar 'ι') = Just U_ι
toMaybeUnicodeLetter (U.LetterChar 'κ') = Just U_κ
toMaybeUnicodeLetter (U.LetterChar 'λ') = Just U_λ
toMaybeUnicodeLetter (U.LetterChar 'μ') = Just U_μ
toMaybeUnicodeLetter (U.LetterChar 'ν') = Just U_ν
toMaybeUnicodeLetter (U.LetterChar 'ξ') = Just U_ξ
toMaybeUnicodeLetter (U.LetterChar 'ο') = Just U_ο
toMaybeUnicodeLetter (U.LetterChar 'π') = Just U_π
toMaybeUnicodeLetter (U.LetterChar 'ρ') = Just U_ρ
toMaybeUnicodeLetter (U.LetterChar 'ς') = Just U_ς
toMaybeUnicodeLetter (U.LetterChar 'σ') = Just U_σ
toMaybeUnicodeLetter (U.LetterChar 'τ') = Just U_τ
toMaybeUnicodeLetter (U.LetterChar 'υ') = Just U_υ
toMaybeUnicodeLetter (U.LetterChar 'φ') = Just U_φ
toMaybeUnicodeLetter (U.LetterChar 'χ') = Just U_χ
toMaybeUnicodeLetter (U.LetterChar 'ψ') = Just U_ψ
toMaybeUnicodeLetter (U.LetterChar 'ω') = Just U_ω
toMaybeUnicodeLetter _ = Nothing

toMaybeUnicodeMark :: U.MarkChar -> Maybe UnicodeMark
toMaybeUnicodeMark (U.MarkChar '\x0300') = Just GraveMark -- COMBINING GRAVE ACCENT
toMaybeUnicodeMark (U.MarkChar '\x0301') = Just AcuteMark -- COMBINING ACUTE ACCENT
toMaybeUnicodeMark (U.MarkChar '\x0308') = Just DiaeresisMark -- COMBINING DIAERESIS
toMaybeUnicodeMark (U.MarkChar '\x0313') = Just SmoothMark -- COMBINING COMMA ABOVE
toMaybeUnicodeMark (U.MarkChar '\x0314') = Just RoughMark -- COMBINING REVERSED COMMA ABOVE
toMaybeUnicodeMark (U.MarkChar '\x0342') = Just CircumflexMark -- COMBINING GREEK PERISPOMENI
toMaybeUnicodeMark (U.MarkChar '\x0345') = Just IotaSubscriptMark -- COMBINING GREEK YPOGEGRAMMENI
toMaybeUnicodeMark _ = Nothing
