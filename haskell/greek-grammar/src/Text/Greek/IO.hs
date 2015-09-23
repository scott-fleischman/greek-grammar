module Text.Greek.IO where

import Control.Lens
import Data.Either
import Text.Greek.FileReference
import Text.Greek.Source.All
import Text.Parsec.Error (ParseError)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U

handleAll :: IO [Work [BasicWord [U.Unit Letter.Info U.UnicodeMark]]]
handleAll = loadAll >>= handleEither
  >>= handleListEither . workToUnitChar
  >>= handleListEither . workToUnitUnicode
  >>= handleListEither . validateWorkFinal . workToLetterInfo

workToUnitChar ::        [Work [BasicWord (T.Text, FileReference)]]
  -> [Either U.UnitError (Work [BasicWord [U.UnitChar]])]
workToUnitChar = fmap $ (workContent . traverse . basicWordSurface) U.toUnitChar

workToUnitUnicode ::        [Work [BasicWord [U.UnitChar]]]
  -> [Either U.UnicodeError (Work [BasicWord [U.UnitUnicode]])]
workToUnitUnicode = fmap $ (workContent . traverse . basicWordSurface . traverse) U.toUnitUnicode

workToLetterInfo
  :: [Work [BasicWord [U.Unit U.UnicodeLetter U.UnicodeMark]]]
  -> [Work [BasicWord [U.Unit Letter.InfoFinal U.UnicodeMark]]]
workToLetterInfo = over (traverse . workContent . traverse . basicWordSurface . traverse . U.unitLetter . _1) Letter.toInfoFinal

validateWorkFinal
  ::   [Work [BasicWord [U.Unit Letter.InfoFinal U.UnicodeMark]]]
  -> [Either
       (ParseError)
       (Work [BasicWord [U.Unit Letter.Info      U.UnicodeMark]])]
validateWorkFinal = fmap $ (workContent . traverse . basicWordSurface) (Letter.parseFinals (^. U.unitLetter . _2 . fileCharReferenceLine) (U.unitLetter . _1))

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
