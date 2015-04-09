module Text.Greek.Phonology.Vowels where

data Vowel = Alpha | Epsilon | Eta | Iota | Omicron | Upsilon | Omega

data VowelLength = Short | Long

data RoundedLip = LipClosed | LipClosedMedium | LipOpen | LipOpenMedium

data TonguePosition = TongueClosed | TongueClosedMedium | TongueOpen | TongueOpenMedium  


data VowelPhoneme =
    VowelPhoneme Vowel VowelLength
  | Diphthong Vowel Vowel
  | ImproperDiphthong Vowel

alpha :: VowelLength -> VowelPhoneme
alpha len = VowelPhoneme Alpha len

omicron :: VowelPhoneme
omicron = VowelPhoneme Omicron Short

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
  , Contraction (Diphthong Omicron Upsilon) omicron omicron
  ]

properties :: [(VowelPhoneme, RoundedLip, TonguePosition)]
properties =
  [ (alpha Long, LipOpen, TongueOpen)
  , (alpha Short, LipOpen, TongueOpen)
  ]
