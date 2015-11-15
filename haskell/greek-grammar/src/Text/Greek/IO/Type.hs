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
  | UnicodeLetterMarks
  | UnicodeLetter
  | UnicodeMark
  | LetterCount
  | MarkCount
  | ConcreteLetterMarks
  | ConcreteLetter
  | ConcreteMark
  | AbstractLetterCaseFinalMarks
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
  | VowelConsonantMarkGroup
  | VowelConsonant
  | Vowel
  | Consonant
  | VowelCount
  | ConsonantCount
  | SyllabicMarkVowelConsonant
  | StartSyllable
  | VocalicSyllableConsonant
  deriving (Eq, Ord)
