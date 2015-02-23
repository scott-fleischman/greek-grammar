{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((.), ($), Show(show), Bool(..))
import Control.Applicative ((<$>))
import Control.Monad (mapM_)
import Data.Default (def)
import Data.Either (Either(..))
import Data.List (filter, (++), maximum)
import Data.Text (Text, append, pack)
import Data.Text.Format (Only(..), Format, format, right, left)
import Data.Text.Format.Params (Params)
import Data.Text.IO (putStrLn)
import Data.Text.Lazy (toStrict)
import Filesystem.Path (FilePath, (</>), (<.>))
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.NewTestament.Bible
import Text.Greek.NewTestament.SBL
import qualified Data.List as L (length)
import qualified Data.Text as T (length)

sblgntPath :: FilePath
sblgntPath = "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

format' :: Params s => Format -> s -> Text
format' fmt ps = toStrict . format fmt $ ps

showBookStats :: [Book] -> Book -> Text
showBookStats bs (Book _ t ss) = format' "{} {} \x00B6s, {} chs, {} vss, {} words" $
  ( right padTitle ' ' t
  , padSegments paragraphFilter
  , padSegments chapterFilter
  , padSegments verseFilter
  , padSegments wordFilter
  )
  where
    padTitle = maximum $ T.length . bookTitle <$> bs
    maxPadSegments f = maximum $ T.length . (\b -> formatSegmentCount (segments b) f) <$> bs
    padSegments f = left (maxPadSegments f) ' ' $ report f
    report = formatSegmentCount ss
    paragraphFilter = \x -> case x of { (SegmentParagraph (Start Paragraph)) -> True ; _ -> False }
    chapterFilter = \x -> case x of { (SegmentChapter (Start (Chapter _))) -> True ; _ -> False }
    verseFilter = \x -> case x of { (SegmentVerse (Start (Verse _))) -> True ; _ -> False }
    wordFilter = \x -> case x of { (SegmentWord _) -> True ; _ -> False }
    formatSegmentCount ss' f = (format' "{}") (Only . L.length . filter f $ ss')

showResults :: Either SBLError Bible -> [Text]
showResults (Left b) = [append "Error: " $ pack . show $ b]
showResults (Right (Bible id title books)) =
  [ id
  , title
  , (format' "Books: {}") (Only . L.length $ books)
  ]
  ++
  (showBookStats books <$> books)

main :: IO ()
main = do
  doc <- readFile def sblgntPath
  let bibleResult = loadOsis doc
  mapM_ putStrLn $ showResults bibleResult
