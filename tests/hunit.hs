{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (readFile)
import Control.Lens
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
import Test.HUnit
import Text.Greek.Conversions
import Text.Greek.Corpus.Bible
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Phonology.Contractions
import Text.Greek.Script
import Text.Greek.Script.Sound
import Text.Greek.Script.Unicode
import Text.XML (readFile)

loadSblgnt = do
  sblgnt <- readFile def sblgntOsisPath
  isRight (loadOsis sblgnt) @?= True

alpha = '\x0391'
alphaAcute = '\x0386'
acute = '\x0301'
smooth = '\x0313'

sampleNounCategory =
  [nounCategory|
    Masculine nouns with stems ending in ο(ς)
         sg: pl:
    nom: ος  οι
    gen: ου  ων
    dat: ῳ   οις
    acc: ον  ους
    voc: ε   οι
    lemmas:
      Ἅγαβος ἄγαμος ἁγιασμός
      τύπος Τύραννος Τυχικός
      ὑάκινθος ὑετός υἱός
      ψευδόχριστος ψιθυρισμός ὦμος
  |]

-- getSampleStem :: Int -> Maybe [Sound]
-- getSampleStem i = listToMaybe . catMaybes . fmap (getStem (sampleNounCategory ^. nounCategoryEndings)) . take 1 . drop i . _nounCategoryWords $ sampleNounCategory

main = defaultMain
  [ testGroup "UnicodeTokenPairs" . pure . testCase "All valid tokens" $
      mapM_ (\p -> assertEqual (showString "'\\x" . showHex (ord . fst $ p) $ "'") [] (validateToken . snd $ p)) unicodeTokenPairs
  , testGroup "SBLGNT" [ testCase "Successful load" loadSblgnt ]
  , testGroup "textToSounds"
    [ testCase "α" $ True @=? case textToSounds "α" of { Right (SingleVowelSound _ : []) -> True ; _ -> False }
    , testCase "β" $ True @=? case textToSounds "β" of { Right (ConsonantSound _ : []) -> True ; _ -> False }
    , testCase "αι" $ True @=? case textToSounds "αι" of { Right (DiphthongSound _ _ : []) -> True ; _ -> False }
    , testCase "αϊ" $ True @=? case textToSounds "αϊ" of { Right (SingleVowelSound _ : SingleVowelSound _ : []) -> True ; _ -> False }
    , testCase "ᾳ" $ True @=? case textToSounds "ᾳ" of { Right (IotaSubscriptVowelSound _ : []) -> True ; _ -> False }
    , testCase "ἁ" $ True @=? case textToSounds "ἁ" of { Right (RoughBreathingSound : SingleVowelSound _ : []) -> True ; _ -> False }
    , testCase "ᾁ" $ True @=? case textToSounds "ᾁ" of { Right (RoughBreathingSound : IotaSubscriptVowelSound _ : []) -> True ; _ -> False }
    , testCase "αἱ" $ True @=? case textToSounds "αἱ" of { Right (RoughBreathingSound : DiphthongSound _ _ : []) -> True ; _ -> False }
    , testCase "invalid" $ True @=? case textToSounds "x" of { Left (InvalidChar 'x') -> True ; _ -> False }
    ]
  , testGroup "Contractions" . pure . testCase "Forward" $
      mapM_ (\t@(v1, v2, vs) -> assertEqual (show t) (sort $ getContractions v1 v2) (sort vs)) forwardContractionTests
  , testGroup "groupMarks"
    [ testCase "alpha" $ groupMarks [alpha] @?= GroupMarksResult [] [LetterMarkGroup alpha []]
    , testCase "alphaAcute_polytonic" $ groupMarks [alphaAcute] @?= GroupMarksResult [] [LetterMarkGroup alphaAcute []]
    , testCase "alpha_acuteMark" $ groupMarks [alpha, acute] @?= GroupMarksResult [] [LetterMarkGroup alpha [acute]]
    , testCase "alpha_smoothMark_acuteMark" $ groupMarks [alpha, smooth, acute] @?= GroupMarksResult [] [LetterMarkGroup alpha [acute, smooth]]
    ]
  , testGroup "removeSuffix"
    [ testCase "empty" $ [1,2,3] @=? removeSuffix [] [1,2,3]
    , testCase "single" $ [1,2] @=? removeSuffix [3] [1,2,3]
    , testCase "all" $ [] @=? removeSuffix [1,2,3] [1,2,3]
    ]
  , testGroup "nounCategory"
    [ testCase "length nounCategoryWords" $ 12 @=? (length $ sampleNounCategory ^. nounCategoryWords)
    , testCase "length nounCategoryToAllForms" $ 120 @=? (length . nounCategoryToAllForms $ sampleNounCategory)
    , testCase "getStem" $ 12 @=? (length . catMaybes . fmap (getStem (sampleNounCategory ^. nounCategoryEndings)) . _nounCategoryWords $ sampleNounCategory)
    ]
  ]
