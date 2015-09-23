module Text.Greek.Script.Syllable where

import Text.Greek.FileReference
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark

data VocalicOpen
  = OneVowel (Letter.Vowel, FileCharReference) (Maybe (Mark.Diaeresis, FileCharReference))
  | IotaSubscriptVowel (Letter.Vowel, FileCharReference) (Mark.IotaSubscript, FileCharReference)
  | TwoVowel (Letter.Vowel, FileCharReference) (Letter.Vowel, FileCharReference)
