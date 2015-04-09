module Text.Greek.Phonology.Vowels where

data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega

data VowelLength = Short | Long

data RoundedLip = Round | Unround

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium  


data VowelPhoneme =
    VowelPhoneme Vowel VowelLength
  | Diphthong Vowel Vowel
  | ImproperDiphthong Vowel
  | SpuriousDiphthong Vowel Vowel

alpha :: VowelLength -> VowelPhoneme
alpha len = VowelPhoneme Alpha len

omicron :: VowelPhoneme
omicron = VowelPhoneme Omicron Short

epsilon :: VowelPhoneme
epsilon = VowelPhoneme Epsilon Short

data Contraction = Contraction
  { target :: VowelPhoneme
  , first :: VowelPhoneme
  , second :: VowelPhoneme
  }

contractions :: [Contraction]
contractions =
  [ Contraction (alpha Long) (alpha Short) (alpha Short)
  , Contraction (alpha Long) (alpha Long) (alpha Short)
  , Contraction (alpha Long) (alpha Short) (alpha Long)
  , Contraction (Diphthong Alpha Iota) (alpha Short) (Diphthong Alpha Iota)
  , Contraction (ImproperDiphthong Alpha) (alpha Short) (ImproperDiphthong Alpha)
  , Contraction (alpha Long) (alpha Short) epsilon
  , Contraction (ImproperDiphthong Alpha) (alpha Short) (Diphthong Epsilon Iota)
  , Contraction (alpha Long) (alpha Short) (SpuriousDiphthong Epsilon Iota)
  , Contraction (Diphthong Omicron Upsilon) omicron omicron
  ]

properties :: [(VowelPhoneme, RoundedLip, TonguePosition)]
properties =
  [ (alpha Long, Unround, TongueOpen)
  , (alpha Short, Unround, TongueOpen)
  , (epsilon Short, Unround, TongueClosedMedium)
  , (eta Long, Unround, TongueOpenMedium)
  , (iota Short, Unround, TongueClosed)
  , (iota Long, Unround, TongueClosed)
  , (omicron Short, Round, TongueClosedMedium)
  , (upsilon Short, Round, TongueClosed)
  , (upsilon Long, Round, TongueClosed)
  , (omega Long, Round, TongueOpenMedium)
  ]
