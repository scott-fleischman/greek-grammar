{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Greek.Corpus.Bible where

import Prelude (Eq, Show, Int, ($), (.), flip, concat, (+), Char, snd, concatMap)
import Data.List (mapAccumL, foldr)
import Data.Map.Strict (fromList, lookup)
import Data.Maybe (Maybe(..))
import Data.Text (Text, unpack)
import Data.Traversable (mapM)
import Control.Monad.State.Lazy (State, evalState, state)
import Text.Greek.Script.Token
import Text.Greek.Script.Unicode

data Bible = Bible
  { bibleId :: Text
  , bibleTitle :: Text
  , bibleBooks :: [Book]
  } deriving (Eq, Show)

data Book = Book
  { bookId :: Text
  , bookTitle :: Text
  , segments :: [Segment]
  } deriving (Eq, Show)

data Segment =
    SegmentParagraph (Milestone Paragraph)
  | SegmentChapter (Milestone Chapter)
  | SegmentVerse (Milestone Verse)
  | SectionTitle Text
  | Separator Text
  | SegmentWord Text
  deriving (Eq, Show)

data (Eq a, Show a) => Milestone a = Start a | End a
  deriving (Eq, Show)

data Paragraph = Paragraph
  deriving (Eq, Show)
data Chapter = Chapter Text
  deriving (Eq, Show)
data Verse = Verse Text
  deriving (Eq, Show)

data Word = Word
  { wordText :: Text
  , wordVerse :: Verse
  , wordIndex :: Int
  } deriving (Eq, Show)

segmentToState :: Segment -> State (Verse, Int) [Word]
segmentToState (SegmentVerse (Start v)) = state $ \(_, i) -> ([], (v, i))
segmentToState (SegmentWord t) = state $ \(v, i) -> ([Word t v i], (v, i + 1))
segmentToState _ = state ([], )

segmentsToWords :: [Segment] -> [Word]
segmentsToWords = concat . flip evalState (Verse "", 0) . mapM segmentToState

data Character = Character
  { character :: Char
  , characterWord :: Word
  , characterIndexInWord :: Int
  , characterIndex :: Int
  } deriving (Eq, Show)

wordsToCharacters :: [Word] -> [Character]
wordsToCharacters = accumIndex (\i (c, w, iiw) -> Character c w iiw i) . concatMap wordToCharacters where

  wordToCharacters w = accumIndex (\i c -> (c, w, i)) (unpack . wordText $ w)

  accumIndex f xs = snd $ mapAccumL (\acc x -> (acc + 1, f acc x)) 0 xs

charactersToTokenContexts :: [Character] -> ([Character], [TokenContext Character])
charactersToTokenContexts = foldr addCharacter ([], [])
  where
    addCharacter c (skipped, ts) = case lookupChar (character c) of
      Just t -> (skipped, (TokenContext t c) : ts)
      Nothing -> (c : skipped, ts)
    lookupChar = flip lookup (fromList unicodeTokenPairs)
