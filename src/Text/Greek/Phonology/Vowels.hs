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
  [ (alpha Long, Unrounded, TongueOpen)
  , (alpha Short, Unrounded, TongueOpen)
  , (epsilon Short, Unrounded, TongueClosedMedium)
  , (eta Long, Unrounded, TongueOpenMedium)
  , (iota Short, Unrounded, TongueClosed)
  , (iota Long, Unrounded, TongueClosed)
  , (omicron Short, Rounded, TongueClosedMedium)
  , (upsilon Short, Rounded, TongueClosed)
  , (upsilon Long, Rounded, TongueClosed)
  , (omega Long, Rounded, TongueOpenMedium)
  ]
