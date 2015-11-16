module Text.Greek.IO.Type where

data Name
  = List Name
  | Function Name Name
  | Indexed Name
  | ReverseIndexed Name
  | Count Name
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
  | Accent
  | Breathing
  | SyllabicMark
  | AbstractLetterMarkKinds
  | MarkGroup
  | AbstractLetterMarkGroup
  | VowelConsonantMarkGroup
  | VowelConsonant
  | Vowel
  | Consonant
  | SyllabicMarkVowelConsonant
  | StartSyllable
  | VocalicSyllableConsonant
  | VocalicSyllableSingle
  | ImproperDiphthong
  | Diphthong
  | Syllable
  | VocalicSyllableConsonantRh
  | ConsonantBreathing
  | ConsonantRh
  | VocalicSyllableConsonantRhCluster
  | ConsonantRhCluster
  deriving (Eq, Ord)
