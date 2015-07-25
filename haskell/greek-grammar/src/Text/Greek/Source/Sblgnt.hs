{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+))
import Conduit
import Control.Lens
import Data.Data
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
  deriving (Show, Eq, Ord, Typeable)

subEvent1 :: X.Event -> Maybe X1_Event
subEvent1  X.EventBeginDocument      = Just $ X1_BeginDocument
subEvent1  X.EventEndDocument        = Just $ X1_EndDocument
subEvent1 (X.EventBeginElement n as) = Just $ X1_BeginElement n as
subEvent1 (X.EventEndElement n)      = Just $ X1_EndElement n
subEvent1 (X.EventContent c)         = Just $ X1_Content c
subEvent1 _                          = Nothing


transform1 :: (FilePath, Maybe P.PositionRange, X.Event) -> Maybe (FilePath, Maybe P.PositionRange, X1_Event)
transform1 = traverseOf _3 subEvent1


removePrefixWith :: Eq b => (a -> b) -> [b] -> [a] -> Maybe [a]
removePrefixWith _ []       ys                  = Just ys
removePrefixWith f (x : xs) (y : ys) | x == f y = removePrefixWith f xs ys
removePrefixWith _ _        _                   = Nothing

removeSuffixWithHelper :: Eq b => (a -> b) -> a -> Maybe ([b], [a]) -> Maybe ([b], [a])
removeSuffixWithHelper _ y (Just ([],     ys))            = Just ([], y : ys)
removeSuffixWithHelper f y (Just (x : xs, ys)) | x == f y = Just (xs,     ys)
removeSuffixWithHelper _ _ _                              = Nothing

removeSuffixWith :: Eq b => (a -> b) -> [b] -> [a] -> Maybe [a]
removeSuffixWith f xs = fmap snd . foldr (removeSuffixWithHelper f) (Just (reverse $ xs, []))





data Result a b = Result { _bad :: a, _good :: b } deriving (Show)
makeLenses ''Result

-- type MapResult a b = Result (Map b [a]) [a]

-- uniqueFoldrHelper :: Ord b => (a -> Maybe b) -> a -> MapResult a b -> MapResult a b
-- uniqueFoldrHelper f x =
--   case f x of
--     Just y  -> good %~ M.insertWith (\_ old -> x : old) y [x]
--     Nothing -> bad %~ (x :)

-- getUniqueResults :: (Foldable m, Ord b) => (a -> Maybe b) -> m a -> MapResult a b
-- getUniqueResults f = foldr (uniqueFoldrHelper f) (Result M.empty [])


resultToEither :: Result [a] b -> Either [a] b
resultToEither (Result [] bs) = Right bs
resultToEither (Result as _ ) = Left as

getResultHelper :: (a -> Maybe b) -> a -> Result [a] [b] -> Result [a] [b]
getResultHelper f x =
  case f x of
    Just y  -> good %~ (y :)
    Nothing -> bad %~ (x :)

getResults :: (Foldable m) => (a -> Maybe b) -> m a -> Either [a] [b]
getResults f = resultToEither . foldr (getResultHelper f) (Result [] [])

