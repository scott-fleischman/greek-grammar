module Text.Greek.IO.Type where

data Name
  = List Name
  | Function Name Name
  | SourceWord
  | WorkSource
  | WorkTitle
  | SourceFile
  | SourceFileLocation
  | ParagraphNumber
  | Elision
  | UnicodeElision
  | UnicodeComposed
  | UnicodeDecomposed
  | UnicodeMarkedLetter
  | UnicodeLetter
  | UnicodeMark
  | LetterCount
  | MarkCount
  deriving (Eq, Ord)
