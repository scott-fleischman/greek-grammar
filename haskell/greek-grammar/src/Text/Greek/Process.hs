module Text.Greek.Process where

import qualified Control.Lens as Lens
import qualified Text.Greek.Json as Json
import qualified Text.Greek.Source.All as All

go :: IO ()
go = All.loadAll >>= handleResult (putStrLn . show . length) . showError

handleResult :: (a -> IO ()) -> Either String a -> IO ()
handleResult _ (Left e) = putStrLn e
handleResult f (Right a) = f a

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson

newtype WorkIndex = WorkIndex { getWorkIndex :: Int } deriving (Eq, Ord, Show)
newtype WordIndex = WordIndex { getWordIndex :: Int } deriving (Eq, Ord, Show)

data WordLocation = WordLocation
  { wordLocationWork :: WorkIndex
  , wordLocationWord :: WordIndex
  } deriving (Eq, Ord, Show)
