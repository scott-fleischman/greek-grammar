{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.Sblgnt where

import Conduit
import Control.Lens
import Data.Map.Lazy (Map)
import Data.XML.Types
import Text.XML
import Text.XML.Stream.Parse
import qualified Data.Map.Lazy as M

readEvents :: FilePath -> IO [EventPos]
readEvents path = runResourceT $ sourceFile path =$= parseBytesPos def $$ sinkList

data EventKind
  = EventKindBeginDocument
  | EventKindEndDocument
  | EventKindBeginElement
  | EventKindEndElement
  | EventKindContent
  deriving (Show, Eq, Ord)

getEventKind :: Event -> Maybe EventKind
getEventKind EventBeginDocument = Just EventKindBeginDocument
getEventKind EventEndDocument = Just EventKindEndDocument
getEventKind (EventBeginElement _ _) = Just EventKindBeginElement
getEventKind (EventEndElement _) = Just EventKindEndElement
getEventKind (EventContent _) = Just EventKindContent
getEventKind _ = Nothing

data Result a b = Result { _good :: a, _bad :: b } deriving (Show)
makeLenses ''Result

uniqueFoldrHelper :: Ord b => (a -> Maybe b) -> a -> Result (Map b [a]) [a] -> Result (Map b [a]) [a]
uniqueFoldrHelper f x =
  case f x of
    Just y  -> good %~ M.insertWith (\_ old -> x : old) y [x]
    Nothing -> bad %~ (x :)

getUniqueResults :: (Foldable m, Ord b) => (a -> Maybe b) -> m a -> Result (Map b [a]) [a]
getUniqueResults f = foldr (uniqueFoldrHelper f) (Result M.empty [])

uniqueEventKinds :: [EventPos] -> Result (Map EventKind [EventPos]) [EventPos]
uniqueEventKinds = getUniqueResults (getEventKind . snd)
