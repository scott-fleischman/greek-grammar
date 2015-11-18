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
  | WordPrefix
  | WordSuffix
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
  | AbstractLetterMarks
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
  | VocalicSyllableABConsonantB
  | VocalicSyllable
  | VocalicSyllableSingle
  | ImproperDiphthong
  | Diphthong
  | Syllable
  | VocalicSyllableABConsonantRh
  | ConsonantBreathing
  | ConsonantRh
  | VocalicSyllableABConsonantRhCluster
  | ConsonantRhCluster
  | ConsonantRhClusterPlace3
  | ConsonantRhClusterPlace3Swap
  | ConsonantRhClusterPlaceInfo
  | ScriptSyllableConsonantRhCluster_Right
  | ScriptSyllableConsonantRhCluster_Approx
  | ScriptSyllableConsonantRhClusterAB_Approx
  deriving (Eq, Ord)
