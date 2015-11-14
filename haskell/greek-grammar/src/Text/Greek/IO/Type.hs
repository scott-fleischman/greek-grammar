module Text.Greek.IO.Type where

data Name
  = List Name
  | Function Name Name
  | Indexed Name
  | ReverseIndexed Name
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
  | ConcreteMarkedLetter
  | ConcreteLetter
  | ConcreteMark
  | AbstractMarkedLetter
  | AbstractLetterCaseFinal
  | AbstractLetter
  | LetterCase
  | LetterFinalForm
  | WordCapitalization
  | MarkKind
  | AbstractLetterMarkKinds
  | MarkGroup
  | AbstractLetterMarkGroup
  | AccentCount
  | BreathingCount
  | SyllabicMarkCount
  deriving (Eq, Ord)
