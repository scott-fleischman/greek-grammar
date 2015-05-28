{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import Data.Char
import Data.Default (def)
import Data.Either
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import Numeric
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Text.Greek.Conversions
import Text.Greek.Corpus.Bible
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Phonology.Contractions
import Text.Greek.Script
import Text.Greek.Script.Sound
import Text.Greek.Script.Unicode
import Text.XML (readFile)

case_valid_tokens = mapM_ (\p -> assertEqual (showString "'\\x" . showHex (ord . fst $ p) $ "'") [] (validateToken . snd $ p)) unicodeTokenPairs

case_load_sblgnt = do
  sblgnt <- readFile def sblgntOsisPath
  isRight (loadOsis sblgnt) @?= True

case_sound_a_vowel = True @=? case textToSounds "α" of { Right (VowelSound (Vowel _) : []) -> True ; _ -> False }
case_sound_b_consonant = True @=? case textToSounds "β" of { Right (ConsonantSound (Consonant _) : []) -> True ; _ -> False }
case_sound_ai_diphthong = True @=? case textToSounds "αι" of { Right (VowelSound (Diphthong _ _) : []) -> True ; _ -> False }
case_sound_ai_diaeresis = True @=? case textToSounds "αϊ" of { Right (VowelSound (Vowel _) : VowelSound (Vowel _) : []) -> True ; _ -> False }
case_sound_ai_iotaSubscript = True @=? case textToSounds "ᾳ" of { Right (VowelSound (ImproperDiphthong _) : []) -> True ; _ -> False }
case_sound_a_rough = True @=? case textToSounds "ἁ" of { Right (ConsonantSound (RoughBreathing _) : VowelSound (Vowel _) : []) -> True ; _ -> False }
case_sound_ai_iotaSubscript_rough = True @=? case textToSounds "ᾁ" of { Right (ConsonantSound (RoughBreathing _) : VowelSound (ImproperDiphthong _) : []) -> True ; _ -> False }
case_sound_ai_diphthong_rough = True @=? case textToSounds "αἱ" of { Right (ConsonantSound (RoughBreathing _) : VowelSound (Diphthong _ _) : []) -> True ; _ -> False }

case_forward_contractions = mapM_ (\t@(v1, v2, vs) -> assertEqual (show t) (sort $ getContractions v1 v2) (sort vs)) forwardContractionTests

alpha = '\x0391'
alphaAcute = '\x0386'
acute = '\x0301'
smooth = '\x0313'

case_groupMarks_alpha = groupMarks [alpha] @?= GroupMarksResult [] [LetterMarkGroup alpha []]
case_groupMarks_alphaAcute_polytonic = groupMarks [alphaAcute] @?= GroupMarksResult [] [LetterMarkGroup alphaAcute []]
case_groupMarks_alpha_acuteMark = groupMarks [alpha, acute] @?= GroupMarksResult [] [LetterMarkGroup alpha [acute]]
case_groupMarks_alpha_smoothMark_acuteMark = groupMarks [alpha, smooth, acute] @?= GroupMarksResult [] [LetterMarkGroup alpha [acute, smooth]]

main :: IO ()
main = $(defaultMainGenerator)
