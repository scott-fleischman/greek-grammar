module Text.Greek.Process where

import qualified Control.Lens as Lens
import qualified Text.Greek.Json as Json
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Word as Word

go :: IO ()
go = All.loadAll >>= handleResult (putStrLn . show . length) . showError

handleResult :: (a -> IO ()) -> Either String a -> IO ()
handleResult _ (Left e) = putStrLn e
handleResult f (Right a) = f a

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson

data WordLocation = WordLocation
  { wordLocationWork :: Work.Index
  , wordLocationWord :: Word.Index
  } deriving (Eq, Ord, Show)
