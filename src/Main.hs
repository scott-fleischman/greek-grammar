{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((.), ($), Bool(..))
import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Lens.Tuple (_1, _2, _3, _4, _5)
import Control.Monad (mapM_)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Functor (fmap)
import Data.List (filter, (++), maximum)
import Data.Text (Text)
import Data.Text.Format (Only(..), Shown(..), right, left)
import Data.Text.Format.Strict (format')
import Data.Text.IO (putStrLn)
import Filesystem.Path (FilePath, (</>), (<.>))
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.NewTestament.Bible
import Text.Greek.NewTestament.SBL
import qualified Data.List as L (length)
import qualified Data.Text as T (length)

sblgntPath :: FilePath
sblgntPath = "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

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

main :: IO ()
main = do
  doc <- readFile def sblgntPath
  let bibleResult = loadOsis doc
  mapM_ putStrLn $ showResults bibleResult
