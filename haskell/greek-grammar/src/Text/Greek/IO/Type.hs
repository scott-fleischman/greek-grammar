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
  | Verse
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
  | ScriptSyllableConsonantRhAB_Right
  | ScriptSyllableConsonantRh_Right
  | ScriptSyllableConsonantRh_Approx
  | ScriptSyllableConsonantRhAB_Approx
  | ListScriptSyllableConsonantRh
  | Crasis
  | ScriptSyllableConsonantRBA_Approx
  | ScriptSyllableConsonantRB_Approx
  | ListScriptSyllableConsonantRB
  | EndOfSentence
  | UnicodeEndOfSentence
  | AcuteCircumflex
  | EndOfSentenceAccent
  | InitialEnclitic
  | WordAccent
  | WordUltimaUnaccented
  | ScriptSyllableConsonantRBAC_Approx
  | MorphgntBook
  | MorphgntChapter
  | MorphgntVerse
  | MorphgntPerson
  | MorphgntTense
  | MorphgntVoice
  | MorphgntMood
  | MorphgntCase
  | MorphgntNumber
  | MorphgntGender
  | MorphgntDegree
  | MorphgntText
  | MorphgntWord
  | MorphgntNormalizedWord
  | MorphgntLemma
  | MorphgntPartOfSpeech
  | MorphgntParsingCode
  deriving (Eq, Ord)
