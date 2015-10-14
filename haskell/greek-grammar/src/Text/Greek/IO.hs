module Text.Greek.IO where

import Prelude hiding (Word)
import Control.Lens
import Data.Either
import Text.Greek.FileReference
import Text.Greek.Source.All
import Text.Parsec.Error (ParseError)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Unit as U
import qualified Text.Greek.Script.Word as Word

handleAll :: IO [Work [Word.Cased [U.UnitLetter Abstract.VowelConsonant Mark.AllPair]]]
handleAll = loadAll >>= handleEither
  >>= mapHandle workToUnitChar
  >>= mapHandle workToConcreteUnit
  >>= mapHandle parseFinalForms . workToCaseLetterFinal
  >>= mapHandle toMarkAll
  >>= mapHandle parseLetterCase
  >>= return . toVowelConsonant

workToUnitChar ::        Work [Word.Basic (T.Text, FileReference)]
  -> Either U.UnitError (Work [Word.Basic [U.UnitChar]])
workToUnitChar = (workContent . traverse . Word.basicSurface) U.toUnitChar

workToConcreteUnit ::       Work [Word.Basic [U.UnitChar]]
  -> Either Concrete.Error (Work [Word.Basic [Concrete.Unit]])
workToConcreteUnit = (workContent . traverse . Word.basicSurface . traverse) Concrete.toUnit

workToCaseLetterFinal
  :: [Work [Word.Basic [U.UnitMarkList Concrete.Letter                       Concrete.Mark]]]
  -> [Work [Word.Basic [U.UnitMarkList (Abstract.Case, Abstract.LetterFinal) Concrete.Mark]]]
workToCaseLetterFinal = over (traverse . workContent . traverse . Word.basicSurface . traverse . U.unitItem . _1) Abstract.toCaseLetterFinal

parseFinalForms ::      Work [Word.Basic [U.UnitMarkList (Abstract.Case, Abstract.LetterFinal) Concrete.Mark]]
  -> Either ParseError (Work [Word.Basic [U.UnitMarkList (Abstract.Case, Abstract.Letter)      Concrete.Mark]])
parseFinalForms = (workContent . traverse . Word.basicSurface) (Abstract.parseFinals (^. U.unitItem . _2 . fileCharReferenceLine) (U.unitItem . _1 . _2))

toMarkAll ::            Work [Word.Basic [U.UnitMarkList (Abstract.Case, Abstract.Letter) Concrete.Mark]]
  -> Either Mark.Error (Work [Word.Basic [U.UnitLetter   (Abstract.Case, Abstract.Letter) Mark.AllPair]])
toMarkAll = (workContent . traverse . Word.basicSurface . traverse . U.unitMarks) Mark.toAllPair


parseLetterCase ::      Work [Word.Basic [U.UnitLetter (Abstract.Case, Abstract.Letter) Mark.AllPair]]
  -> Either ParseError (Work [Word.Cased [U.UnitLetter                 Abstract.Letter  Mark.AllPair]])
parseLetterCase = (workContent . traverse) parseWordLetterCase

parseWordLetterCase ::  Word.Basic [U.UnitLetter (Abstract.Case, Abstract.Letter) Mark.AllPair]
  -> Either ParseError (Word.Cased [U.UnitLetter                 Abstract.Letter  Mark.AllPair])
parseWordLetterCase w = do
  (c, s) <- parseLetterCaseWork (w ^. Word.basicSurface)
  return $ Word.Cased (dropUnit s) (w ^. Word.basicElision) c
    where
      dropUnit
        :: [U.UnitLetter ((), Abstract.Letter) m]
        -> [U.UnitLetter      Abstract.Letter  m]
      dropUnit = over (traverse . U.unitItem . _1) snd

parseLetterCaseWork ::                      [U.UnitLetter (Abstract.Case, Abstract.Letter) Mark.AllPair]
  -> Either ParseError (Word.IsCapitalized, [U.UnitLetter ((),            Abstract.Letter) Mark.AllPair])
parseLetterCaseWork = Abstract.parseCase (^. U.unitItem . _2 . fileCharReferenceLine) (U.unitItem . _1 . _1)


parseVocalicSyllable :: Work [Word.Cased [U.UnitLetter Abstract.VowelConsonant     Mark.AllPair]]
  -> Either ParseError (Work [Word.Cased [U.Unit       Syllable.VocalicConsonant Mark.AccentBreathingAllPair]])
parseVocalicSyllable = (workContent . traverse . Word.casedSurface) Syllable.parseVocalicSyllable


toVowelConsonant
  :: [Work [Word.Cased [U.UnitLetter Abstract.Letter         Mark.AllPair]]]
  -> [Work [Word.Cased [U.UnitLetter Abstract.VowelConsonant Mark.AllPair]]]
toVowelConsonant = over (traverse . workContent . traverse . Word.casedSurface . traverse . U.unitItem . _1) Abstract.toVowelConsonant


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
