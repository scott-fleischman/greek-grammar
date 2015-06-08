{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Corpus.Bible.Stats where

import Prelude hiding (words)
import Control.Lens ((^.))
import Control.Lens.Tuple (_1, _2, _3, _4, _5)
import Data.Text (Text)
import Data.Text.Format (Only(..), Shown(..), right, left)
import Data.Text.Format.Strict (format')
import Text.Greek.Corpus.Bible
import Text.Greek.Conversions
import Text.Greek.Morphology.Noun
import Text.Greek.Script.Sound
import Text.Greek.Mounce.Morphology
import qualified Data.Text as T

getBookStats :: Book -> (Text, Text, Text, Text, Text)
getBookStats (Book _ t ss) = (t, count paragraphs, count chapters, count verses, count words)
  where
    count f = (format' "{}") (Only . length . filter f $ ss)
    paragraphs = \x -> case x of { (SegmentParagraph (Start Paragraph)) -> True ; _ -> False }
    chapters = \x -> case x of { (SegmentChapter (Start (Chapter _))) -> True ; _ -> False }
    verses = \x -> case x of { (SegmentVerse (Start (Verse _))) -> True ; _ -> False }
    words = \x -> case x of { (SegmentWord _) -> True ; _ -> False }

showErrorContinue :: Show b => (a -> [Text]) -> (Either b a) -> [Text]
showErrorContinue _ (Left e) = [format' "Error: {}" (Only $ Shown e)]
showErrorContinue f (Right b) = f b

showResults :: Bible -> [Text]
showResults (Bible i title books) =
  [ i
  , title
  , (format' "Books: {}") (Only . length $ books)
  ]
  ++
  (fmap (\s -> format' "{} {} \x00B6s, {} chs, {} vss, {} words" (t s, leftPad _2 s, leftPad _3 s, leftPad _4 s, leftPad _5 s)) stats)
  where
    stats = getBookStats <$> books
    maxPad g = maximum $ (\s -> T.length $ s ^. g) <$> stats
    t s = right (maximum $ (\s' -> T.length $ s' ^. _1) <$> stats) ' ' (s ^. _1)
    leftPad g s = left (maxPad g) ' ' (s ^. g)

matchNouns :: Bible -> [Text]
matchNouns b = (showPair . toSoundVersePair) <$> words
  where
    showPair (Verse v, t, ss) = format' "{} {} {}" (v, t, T.intercalate "; " (showErrorContinue getNounMatchesText ss))
    toSoundVersePair w = (wordVerse w, wordText w, getWordSounds w)
    getWordSounds = textToSounds . T.filter nonPunctuation . wordText
    nonPunctuation c = c /= '\x2019' && c /= '\x1FBD'
    words = segmentsToWords . concat . fmap segments . bibleBooks $ b

getNounMatchesText :: [Sound] -> [Text]
getNounMatchesText _ = ["matches"]

getNounMatches :: [Sound] -> [NounForm]
getNounMatches _ = [NounForm [] Nominative Singular "Sample nouns"]
