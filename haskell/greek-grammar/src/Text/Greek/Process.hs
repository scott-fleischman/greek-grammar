module Text.Greek.Process where

import Data.Text (Text)
import Text.Greek.FileReference (FileReference)
import qualified Control.Lens as Lens
import qualified Text.Greek.Json as Json
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Word as Word

go :: IO ()
go = All.loadAll >>= handleResult process . showError

process :: [Work.Indexed [Word.Indexed (Text, FileReference)]] -> IO ()
process = putStrLn . show . length

getSourceTexts :: [Work.Indexed [Word.Indexed (Text, FileReference)]] -> [(WordLocation, Text)]
getSourceTexts = concatMap getIndexedWorkProps
  where
    getIndexedWorkProps :: Work.Indexed [Word.Indexed (Text, FileReference)] -> [(WordLocation, Text)]
    getIndexedWorkProps w = fmap (\(i, p) -> ((getWorkIndex w, i), p)) (getWorkProps w)

    getWorkProps :: Work.Indexed [Word.Indexed (Text, FileReference)] -> [(Word.Index, Text)]
    getWorkProps = fmap getIndexedWordProp . Work.getContent

    getWorkIndex :: Work.Indexed a -> Work.Index
    getWorkIndex = Lens.view (Work.info . Lens._1)

    getIndexedWordProp :: Word.Indexed (Text, FileReference) -> (Word.Index, Text)
    getIndexedWordProp w = (getWordIndex w, getWordProp w)

    getWordIndex :: Word.Indexed a -> Word.Index
    getWordIndex = Lens.view (Word.info . Lens._1)

    getWordProp :: Word.Indexed (Text, FileReference) -> Text
    getWordProp = Lens.view (Word.surface . Lens._1)

handleResult :: (a -> IO ()) -> Either String a -> IO ()
handleResult _ (Left e) = putStrLn e
handleResult f (Right a) = f a

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson

type WordLocation = (Work.Index, Word.Index)
