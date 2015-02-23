{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((.), ($), Show(show), Bool(..))
import Control.Applicative ((<$>))
import Control.Monad (mapM_)
import Data.Default (def)
import Data.Either (Either(..))
import Data.List (filter, length, (++))
import Data.Text (Text, append, concat, pack)
import Data.Text.IO (putStrLn)
import Filesystem.Path (FilePath, (</>), (<.>))
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.NewTestament.Bible
import Text.Greek.NewTestament.SBL

sblgntPath :: FilePath
sblgntPath = "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

showBookStats :: Book -> Text
showBookStats (Book _ t ss) = concat
  [ t
  , ": "
  , report $ \x -> case x of { (SegmentParagraph (Start Paragraph)) -> True ; _ -> False }
  , " \x00B6s, "
  , report $ \x -> case x of { (SegmentChapter (Start (Chapter _))) -> True ; _ -> False }
  , " chs, "
  , report $ \x -> case x of { (SegmentVerse (Start (Verse _))) -> True ; _ -> False }
  , " vss, "
  , report $ \x -> case x of { (SegmentWord _) -> True ; _ -> False }
  , " words"
  ]
  where report f = pack . show . length $ filter f ss

showResults :: Either SBLError Bible -> [Text]
showResults (Left b) = [append "Error: " $ pack . show $ b]
showResults (Right (Bible id title books)) =
  [ id
  , title
  , append "Books: " (pack . show $ length books)
  ]
  ++
  (showBookStats <$> books)

main :: IO ()
main = do
  doc <- readFile def sblgntPath
  let bibleResult = loadOsis doc
  mapM_ putStrLn $ showResults bibleResult
