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
import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U
import qualified Text.Greek.Script.Word as Word

handleAll :: IO [Work [Word.Cased [U.UnitLetter Letter.VowelConsonant (Mark.AccentBreathingAllPair, Maybe Mark.SyllabicAllPair)]]]
handleAll = loadAll >>= handleEither
  >>= mapHandle workToUnitChar
  >>= mapHandle workToUnitUnicode
  >>= mapHandle parseFinalForms . workToCaseLetterFinal
  >>= mapHandle toMarkAll
  >>= mapHandle parseLetterCase
  >>= return . partitionSyllabicAllPair . toVowelConsonant

workToUnitChar ::        Work [Word.Basic (T.Text, FileReference)]
  -> Either U.UnitError (Work [Word.Basic [U.UnitChar]])
workToUnitChar = (workContent . traverse . Word.basicSurface) U.toUnitChar

workToUnitUnicode ::        Work [Word.Basic [U.UnitChar]]
  -> Either U.UnicodeError (Work [Word.Basic [U.UnitUnicode]])
workToUnitUnicode = (workContent . traverse . Word.basicSurface . traverse) U.toUnitUnicode

workToCaseLetterFinal
  :: [Work [Word.Basic [U.UnitMarkList U.UnicodeLetter                   U.UnicodeMark]]]
  -> [Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.LetterFinal) U.UnicodeMark]]]
workToCaseLetterFinal = over (traverse . workContent . traverse . Word.basicSurface . traverse . U.unitItem . _1) Letter.toCaseLetterFinal

parseFinalForms ::      Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.LetterFinal) U.UnicodeMark]]
  -> Either ParseError (Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.Letter)      U.UnicodeMark]])
parseFinalForms = (workContent . traverse . Word.basicSurface) (Letter.parseFinals (^. U.unitItem . _2 . fileCharReferenceLine) (U.unitItem . _1 . _2))

toMarkAll ::            Work [Word.Basic [U.UnitMarkList (Letter.Case, Letter.Letter) U.UnicodeMark]]
  -> Either Mark.Error (Work [Word.Basic [U.UnitLetter   (Letter.Case, Letter.Letter) Mark.AllPair]])
toMarkAll = (workContent . traverse . Word.basicSurface . traverse . U.unitMarks) Mark.toAllPair


parseLetterCase ::      Work [Word.Basic [U.UnitLetter (Letter.Case, Letter.Letter) Mark.AllPair]]
  -> Either ParseError (Work [Word.Cased [U.UnitLetter               Letter.Letter  Mark.AllPair]])
parseLetterCase = (workContent . traverse) parseWordLetterCase

parseWordLetterCase ::  Word.Basic [U.UnitLetter (Letter.Case, Letter.Letter) Mark.AllPair]
  -> Either ParseError (Word.Cased [U.UnitLetter               Letter.Letter  Mark.AllPair])
parseWordLetterCase w = do
  (c, s) <- parseLetterCaseWork (w ^. Word.basicSurface)
  return $ Word.Cased (dropUnit s) (w ^. Word.basicElision) c
    where
      dropUnit
        :: [U.UnitLetter ((), Letter.Letter) m]
        -> [U.UnitLetter      Letter.Letter  m]
      dropUnit = over (traverse . U.unitItem . _1) snd

parseLetterCaseWork ::                      [U.UnitLetter (Letter.Case, Letter.Letter) Mark.AllPair]
  -> Either ParseError (Word.IsCapitalized, [U.UnitLetter ((),          Letter.Letter) Mark.AllPair])
parseLetterCaseWork = Letter.parseCase (^. U.unitItem . _2 . fileCharReferenceLine) (U.unitItem . _1 . _1)


parseVocalicSyllable :: [Work [Word.Cased [U.UnitLetter Letter.VowelConsonant     (Mark.AccentBreathingAllPair, Maybe Mark.SyllabicAllPair)]]]
  -> Either ParseError  [Work [Word.Cased [U.UnitLetter Syllable.VocalicConsonant (Mark.AccentBreathingAllPair, ()                        )]]]
parseVocalicSyllable = undefined


toVowelConsonant
  :: [Work [Word.Cased [U.UnitLetter Letter.Letter         Mark.AllPair]]]
  -> [Work [Word.Cased [U.UnitLetter Letter.VowelConsonant Mark.AllPair]]]
toVowelConsonant = over (traverse . workContent . traverse . Word.casedSurface . traverse . U.unitItem . _1) Letter.toVowelConsonant

partitionSyllabicAllPair
  :: [Work [Word.Cased [U.UnitLetter Letter.VowelConsonant Mark.AllPair]]]
  -> [Work [Word.Cased [U.UnitLetter Letter.VowelConsonant (Mark.AccentBreathingAllPair, Maybe Mark.SyllabicAllPair)]]]
partitionSyllabicAllPair = over (traverse . workContent . traverse . Word.casedSurface . traverse . U.unitMarks) Mark.partitionSyllabicAllPair


printErrors :: (Show e, Foldable t) => t e -> IO a
printErrors es = do
  mapM_ (T.putStrLn . T.pack . show) es
  fail "failure"

handleEither :: (Show e, Foldable t) => Either (t e) x -> IO x
handleEither (Left es) = printErrors es
handleEither (Right x) = return x

mapHandle :: (Show e) => (a -> Either e b) -> [a] -> IO [b]
mapHandle f = handleListEither . fmap f

handleListEither :: (Show e) => [Either e x] -> IO [x]
handleListEither eithers = case errors of
  _ : _ -> printErrors errors
  [] -> return results
  where
    (errors, results) = partitionEithers eithers
