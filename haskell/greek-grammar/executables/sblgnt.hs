{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ((.), ($), Bool(..), (==), (/=), (&&), Int, (>>=), show)
import qualified Prelude as Unsafe ((!!))
import Control.Applicative (pure)
import Control.Lens hiding ((<.>))
import Control.Monad (mapM_, Monad(..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Functor (fmap)
import Data.List (filter, intersperse, concat, length)
import Data.Maybe
import Data.Text (Text, replace, unpack)
import Data.Text.Format (Only(..))
import Data.Text.Format.Strict (format')
import Data.Text.IO (putStrLn, writeFile)
import System.FilePath
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.Corpus.Bible
import Text.Greek.Corpus.Bible.Stats
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Source.Sblgnt
import Text.Greek.Utility
import qualified Text.Greek.Source.Osis as Osis
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P

load :: IO (Either SBLError Bible)
load = do
  doc <- readFile def sblgntOsisPath
  return $ loadOsis doc

report :: (Bible -> [Text]) -> IO ()
report f = load >>= mapM_ putStrLn . showErrorContinue f

mainDocumentToOsisText :: IO ()
mainDocumentToOsisText = do
  doc <- readFile def sblgntOsisPath
  let osis = Osis.documentToOsisText doc
  putStrLn . T.pack . show $ osis

mainDocumentToXml :: IO ()
mainDocumentToXml = do
  events <- readEvents' sblgntXmlPath
  let results = tf2p events
  putStrLn . T.pack . show . (_Right . _2 %~ length) $ results

xmlTransform
  :: FilePath * [Maybe P.PositionRange * XmlEventAll]
  -> [ErrorMessage] + [FileReference * XmlEventAll]
xmlTransform x =
      tf2p x
  >>= tf3p
  >>= tf4p
  >>. tf5


main :: IO ()
main = mainDocumentToXml

matchNounsIO :: IO ()
matchNounsIO = report matchNouns

dumpBible :: IO ()
dumpBible = report showResults

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
      writeFile (agdaSblgntPath <.> "agda") (indexAgda bible)
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
writeBookAgda book = writeFile (agdaSblgntPath </> unpack (bookId book) <.> "agda") (bookToAgda book)

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
