{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((.), ($), Bool(..), (==), (/=), (&&), Int)
import qualified Prelude as Unsafe ((!!))
import Control.Applicative ((<$>), pure)
import Control.Lens ((^.))
import Control.Lens.Tuple (_1, _2, _3, _4, _5)
import Control.Monad (mapM_, Monad(..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Functor (fmap)
import Data.List (filter, (++), maximum, intersperse, concat)
import Data.Text (Text, replace, unpack)
import Data.Text.Format (Only(..), Shown(..), right, left)
import Data.Text.Format.Strict (format')
import Data.Text.IO (putStrLn, writeFile)
import Filesystem.Path.CurrentOS ((</>), (<.>), encodeString, fromText)
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.Corpus.Bible
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import qualified Data.List as L (length)
import qualified Data.Text as T (length, concat)

getBookStats :: Book -> (Text, Text, Text, Text, Text)
getBookStats (Book _ t ss) = (t, count paragraphs, count chapters, count verses, count words)
  where
    count f = (format' "{}") (Only . L.length . filter f $ ss)
    paragraphs = \x -> case x of { (SegmentParagraph (Start Paragraph)) -> True ; _ -> False }
    chapters = \x -> case x of { (SegmentChapter (Start (Chapter _))) -> True ; _ -> False }
    verses = \x -> case x of { (SegmentVerse (Start (Verse _))) -> True ; _ -> False }
    words = \x -> case x of { (SegmentWord _) -> True ; _ -> False }

showResults :: Either SBLError Bible -> [Text]
showResults (Left b) = [format' "Error: {}" (Only $ Shown b)]
showResults (Right (Bible id title books)) =
  [ id
  , title
  , (format' "Books: {}") (Only . L.length $ books)
  ]
  ++
  (fmap (\s -> format' "{} {} \x00B6s, {} chs, {} vss, {} words" (t s, leftPad _2 s, leftPad _3 s, leftPad _4 s, leftPad _5 s)) stats)
  where
    stats = getBookStats <$> books
    max g = maximum $ (\s -> T.length $ s ^. g) <$> stats
    t s = right (maximum $ (\s' -> T.length $ s' ^. _1) <$> stats) ' ' (s ^. _1)
    leftPad g s = left (max g) ' ' (s ^. g)

load :: IO (Either SBLError Bible)
load = do
  doc <- readFile def sblgntOsisPath
  return $ loadOsis doc

main :: IO ()
main = writeSblgntAgda

dumpBible :: IO ()
dumpBible = do
 bibleResult <- load
 mapM_ putStrLn $ showResults bibleResult

-- for working in GHCi
getBook :: Int -> IO (Book)
getBook n = load >>= \ (Right bible) -> return . (Unsafe.!! n) . bibleBooks $ bible

dumpWords :: Int -> IO ()
dumpWords bookIndex = do
  book <- getBook bookIndex
  mapM_ (\(Word t (Verse v) _) -> putStrLn $ format' "{}\t\t{}" (t, v)) $ segmentsToWords . segments $ book

dumpCharacters :: Int -> IO ()
dumpCharacters bookIndex = do
  book <- getBook bookIndex
  mapM_ (\(Character c (Word t _ _) _ _) -> putStrLn $ format' "{} {}" (c, t)) $ wordsToCharacters . segmentsToWords . segments $ book

writeSblgntAgda :: IO ()
writeSblgntAgda = do
  bibleResult <- load
  case bibleResult of
    Left _ -> putStrLn "Error"
    Right bible -> do
      writeFile (encodeString $ agdaSblgntPath <.> "agda") (indexAgda bible)
      mapM_ writeBookAgda (bibleBooks bible)

indexAgda :: Bible -> Text
indexAgda bible = format' "module Text.Greek.SBLGNT where\n\
\\n\
\open import Data.List\n\
\open import Text.Greek.Bible\n\
\{}\
\\n\
\books : List (List (Word))\n\
\books =\n\
\    {}\n\
\  ∷ []\n\
\" (is, bs) where
  is = T.concat . fmap (\b -> format' "open import Text.Greek.SBLGNT.{}\n" (Only $ bookId b)) $ books
  bs = T.concat . intersperse "\n  ∷ " . fmap (agdaBookName . bookTitle) $ books
  books = bibleBooks bible

writeBookAgda :: Book -> IO ()
writeBookAgda book = writeFile (encodeString $ agdaSblgntPath </> fromText (bookId book) <.> "agda") (bookToAgda book)

bookToAgda :: Book -> Text
bookToAgda (Book i t ss) = format' "module Text.Greek.SBLGNT.{} where\n\
\\n\
\open import Data.List\n\
\open import Text.Greek.Bible\n\
\open import Text.Greek.Script\n\
\open import Text.Greek.Script.Unicode\n\
\\n\
\{} : List (Word)\n\
\{} =\n\
\    {}\n\
\  ∷ []\n\
\" (i, agdaBook, agdaBook, wt) where
  wt = (T.concat . intersperse "\n  ∷ " . fmap wordToAgda . segmentsToWords $ ss)
  agdaBook = agdaBookName t

agdaBookName :: Text -> Text
agdaBookName b = replace " " "-" b

wordToAgda :: Word -> Text
wordToAgda (Word wt (Verse v) _) = format' "word ({}) \"{}\"" (wordTextToAgda wt, v)

wordTextToAgda :: Text -> Text
wordTextToAgda wt = format' "{} ∷ []" (Only . concat . intersperse " ∷ " . fmap escapeLambda . fmap pure . filter (\c -> c /= '’' && c /= '᾽') $ (unpack wt))
  where
    escapeLambda c = case c == "λ" of
      True -> "∙λ"
      False -> c
