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
  | ConcreteMarkedLetter
  | ConcreteLetter
  | ConcreteMark
  | AbstractMarkedLetter
  | AbstractLetterCaseFinal
  | AbstractLetter
  | LetterCase
  | LetterFinalForm
  | MarkKind
  | AbstractLetterCaseFinalMarkKind
  | MarkGroup
  | AbstractLetterCaseFinalMarkGroup
  deriving (Eq, Ord)
