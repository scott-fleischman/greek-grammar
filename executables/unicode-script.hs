{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (($), return, (.), (==), putStrLn, Show(..))
import Data.Foldable (concatMap)
import Data.List (length)
import Data.Maybe (Maybe(..), maybeToList)
import Data.Text (Text, lines, split)
import Data.Text.IO (readFile)
import Filesystem.Path.CurrentOS (FilePath, (</>), (<.>), encodeString)
import System.IO (IO)

-- Property Type Symbol  Examples
-- Catalog C Age, Block
-- Enumeration E Joining_Type, Line_Break
-- Binary  B Uppercase, White_Space
-- String  S Uppercase_Mapping, Case_Folding
-- Numeric N Numeric_Value
-- Miscellaneous M Name, Jamo_Short_Name

-- Third Column. This column indicates the status of the property: Normative or Informative or Contributory or Provisional.

ucdPath :: FilePath
ucdPath = "data" </> "ucd" </> "UnicodeData" <.> "txt"

data TextRecord = TextRecord
  { textCodePoint :: Text
  , textName :: Text
  , textGeneralCategory :: Text
  , textCanonicalCombiningClass :: Text
  , textBidiClass :: Text
  , textDecompositionMapping :: Text
  , textNumericType6 :: Text
  , textNumericType7 :: Text
  , textNumericType8 :: Text
  , textBidiMirrored :: Text
  , textUnicode1NameObsolete :: Text
  , textIsoCommentObsolete :: Text
  , textSimpleUppercaseMapping :: Text
  , textSimpleLowercaseMapping :: Text
  , textSimpleTitlecaseMapping :: Text
  }

toSplitTextRecord :: [Text] -> Maybe TextRecord
toSplitTextRecord (f0 : f1 : f2 : f3 : f4 : f5 : f6 : f7 : f8 : f9 : f10 : f11 : f12 : f13 : f14 : []) = Just $ TextRecord f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14
toSplitTextRecord _ = Nothing

toTextRecord :: Text -> Maybe TextRecord
toTextRecord = toSplitTextRecord . split (== ';')

toTextRecords :: Text -> [TextRecord]
toTextRecords = concatMap (maybeToList . toTextRecord) . lines

main :: IO ()
main = do
  content <- readFile $ encodeString ucdPath
  let records = toTextRecords content
  putStrLn . show $ length records
  return ()
