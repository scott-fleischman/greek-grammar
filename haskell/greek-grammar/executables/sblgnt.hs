{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (Word)
import Control.Lens hiding ((<.>))
import Data.Default (def)
import Data.List
import Data.Text (Text)
import Data.Text.Format (Only(..))
import Data.Text.Format.Strict (format')
import System.FilePath
import Text.Greek.Corpus.Bible
import Text.Greek.Corpus.Bible.Stats
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.XML as X
import qualified Text.Greek.Source.Sblgnt as S

load :: IO (Either SBLError Bible)
load = do
  doc <- X.readFile def sblgntOsisPath
  return $ loadOsis doc

report :: (Bible -> [Text]) -> IO ()
report f = load >>= mapM_ T.putStrLn . showErrorContinue f

mainDocumentToXml :: IO ()
mainDocumentToXml = do
  result <- S.readSblgntEvents sblgntXmlPath
  case result of
    Left es -> mapM_ (T.putStrLn . T.pack . show) es
    Right sblgnt -> T.putStrLn . T.pack . show . length . S.sblgntBooks $ sblgnt

main :: IO ()
main = mainDocumentToXml

matchNounsIO :: IO ()
matchNounsIO = report matchNouns

dumpBible :: IO ()
dumpBible = report showResults

-- for working in GHCi
getBook :: Int -> IO (Book)
getBook n = load >>= \ (Right bible) -> return . (!! n) . bibleBooks $ bible

dumpWords :: Int -> IO ()
dumpWords bookIndex = do
  book <- getBook bookIndex
  mapM_ (\(Word t (Verse v) _) -> T.putStrLn $ format' "{}\t\t{}" (t, v)) $ segmentsToWords . segments $ book

dumpCharacters :: Int -> IO ()
dumpCharacters bookIndex = do
  book <- getBook bookIndex
  mapM_ (\(Character c (Word t _ _) _ _) -> T.putStrLn $ format' "{} {}" (c, t)) $ wordsToCharacters . segmentsToWords . segments $ book

writeSblgntAgda :: IO ()
writeSblgntAgda = do
  bibleResult <- load
  case bibleResult of
    Left _ -> T.putStrLn "Error"
    Right bible -> do
      T.writeFile (agdaSblgntPath <.> "agda") (indexAgda bible)
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
writeBookAgda book = T.writeFile (agdaSblgntPath </> T.unpack (bookId book) <.> "agda") (bookToAgda book)

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
agdaBookName b = T.replace " " "-" b

wordToAgda :: Word -> Text
wordToAgda (Word wt (Verse v) _) = format' "word ({}) \"{}\"" (wordTextToAgda wt, v)

wordTextToAgda :: Text -> Text
wordTextToAgda wt = format' "{} ∷ []" (Only . concat . intersperse " ∷ " . fmap escapeLambda . fmap pure . filter (\c -> c /= '’' && c /= '᾽') $ (T.unpack wt))
  where
    escapeLambda c = case c == "λ" of
      True -> "∙λ"
      False -> c
