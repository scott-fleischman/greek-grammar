module Text.Greek.IO where

import Prelude hiding (Word)
import Control.Lens
import Data.Either
import Text.Greek.FileReference
import Text.Greek.Source.All
import Text.Parsec.Error (ParseError)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U
import qualified Text.Greek.Script.Word as Word

handleAll :: IO [Work [Word.Basic [U.Unit (Letter.Case, (Letter.Letter, Letter.IsLast)) Mark.AllPair]]]
handleAll = loadAll >>= handleEither
  >>= handleListEither . workToUnitChar
  >>= handleListEither . workToUnitUnicode
  >>= handleListEither . parseFinalForms . workToCaseLetterFinal
  >>= handleListEither . toMarkAll

workToUnitChar ::        [Work [Word.Basic (T.Text, FileReference)]]
  -> [Either U.UnitError (Work [Word.Basic [U.UnitChar]])]
workToUnitChar = fmap $ (workContent . traverse . Word.basicSurface) U.toUnitChar

workToUnitUnicode ::        [Work [Word.Basic [U.UnitChar]]]
  -> [Either U.UnicodeError (Work [Word.Basic [U.UnitUnicode]])]
workToUnitUnicode = fmap $ (workContent . traverse . Word.basicSurface . traverse) U.toUnitUnicode

workToCaseLetterFinal
  :: [Work [Word.Basic [U.UnitMarkList U.UnicodeLetter            U.UnicodeMark]]]
  -> [Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.LetterFinal) U.UnicodeMark]]]
workToCaseLetterFinal = over (traverse . workContent . traverse . Word.basicSurface . traverse . U.unitLetter . _1) Letter.toCaseLetterFinal

parseFinalForms
  ::   [Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.LetterFinal)             U.UnicodeMark]]]
  -> [Either
       ParseError
       (Work [Word.Basic [U.UnitMarkList (Letter.Case, (Letter.Letter, Letter.IsLast)) U.UnicodeMark]])]
parseFinalForms = fmap $ (workContent . traverse . Word.basicSurface) (Letter.parseFinals (^. U.unitLetter . _2 . fileCharReferenceLine) (U.unitLetter . _1 . _2))

toMarkAll
  ::   [Work [Word.Basic [U.UnitMarkList (Letter.Case, (Letter.Letter, Letter.IsLast)) U.UnicodeMark]]]
  -> [Either
       Mark.Error
       (Work [Word.Basic [U.Unit         (Letter.Case, (Letter.Letter, Letter.IsLast)) Mark.AllPair]])]
toMarkAll = fmap $ (workContent . traverse . Word.basicSurface . traverse . U.unitMarks) Mark.toAllPair

printErrors :: (Show e, Foldable t) => t e -> IO a
printErrors es = do
  mapM_ (T.putStrLn . T.pack . show) es
  fail "failure"

handleEither :: (Show e, Foldable t) => Either (t e) x -> IO x
handleEither (Left es) = printErrors es
handleEither (Right x) = return x

handleListEither :: (Show e) => [Either e x] -> IO [x]
handleListEither eithers = case errors of
  _ : _ -> printErrors errors
  [] -> return results
  where
    (errors, results) = partitionEithers eithers
