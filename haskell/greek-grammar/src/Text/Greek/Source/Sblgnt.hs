{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+))
import Conduit
import Control.Lens
import Data.Map.Lazy (Map)
import Data.Text (Text)
import qualified Data.Conduit.Attoparsec as P
import qualified Data.Map.Lazy as M
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as P

addFilePath :: FilePath -> (a, b) -> (FilePath, a, b)
addFilePath p (x, y) = (p, x, y)

readEvents :: FilePath -> IO [(FilePath, Maybe P.PositionRange, X.Event)]
readEvents path = eventList >>= return . fmap (addFilePath path)
  where eventList = runResourceT $ sourceFile path =$= P.parseBytesPos P.def $$ sinkList


data X1_Event
  = X1_BeginDocument
  | X1_EndDocument
  | X1_BeginElement X.Name [(X.Name, [X.Content])]
  | X1_EndElement X.Name
  | X1_Content X.Content
  deriving (Show, Eq, Ord)

subEvent1 :: X.Event -> Maybe X1_Event
subEvent1  X.EventBeginDocument      = Just $ X1_BeginDocument
subEvent1  X.EventEndDocument        = Just $ X1_EndDocument
subEvent1 (X.EventBeginElement n as) = Just $ X1_BeginElement n as
subEvent1 (X.EventEndElement n)      = Just $ X1_EndElement n
subEvent1 (X.EventContent c)         = Just $ X1_Content c
subEvent1 _                          = Nothing


removePrefix :: Eq a => [a] -> [a] -> Maybe [a]
removePrefix [] ys = Just ys
removePrefix _  [] = Nothing
removePrefix (x : xs) (y : ys)
  | x == y    = removePrefix xs ys
  | otherwise = Nothing

removeSuffixHelper :: Eq a => a -> (Maybe [a], Maybe [a]) -> (Maybe [a], Maybe [a])
removeSuffixHelper y (Just [],       Just ys)          = (Just [], Just $ y : ys)
removeSuffixHelper y (Just (x : xs), Just ys) | x == y = (Just xs, Just $     ys)
removeSuffixHelper _ _ = (Nothing, Nothing)

removeSuffix :: Eq a => [a] -> [a] -> Maybe [a]
removeSuffix xs = snd . foldr removeSuffixHelper (Just . reverse $ xs, Just [])



data Result a b = Result { _good :: a, _bad :: b } deriving (Show)
makeLenses ''Result

type MapResult a b = Result (Map b [a]) [a]

uniqueFoldrHelper :: Ord b => (a -> Maybe b) -> a -> MapResult a b -> MapResult a b
uniqueFoldrHelper f x =
  case f x of
    Just y  -> good %~ M.insertWith (\_ old -> x : old) y [x]
    Nothing -> bad %~ (x :)

getUniqueResults :: (Foldable m, Ord b) => (a -> Maybe b) -> m a -> MapResult a b
getUniqueResults f = foldr (uniqueFoldrHelper f) (Result M.empty [])
