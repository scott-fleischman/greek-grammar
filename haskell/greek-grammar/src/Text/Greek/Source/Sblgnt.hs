{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), error)
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

readEvents :: FilePath -> IO [(Maybe P.PositionRange, X.Event)]
readEvents fp = runResourceT $ sourceFile fp =$= P.parseBytesPos P.def $$ sinkList


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


newtype ErrorMessage = ErrorMessage { getErrorMessage :: String }
data Error a = Error
  { _errorContext :: a
  , _errorMessage :: ErrorMessage
  }
makeLenses ''Error

maybeToError :: c -> String -> Maybe a -> Either (Error c) a
maybeToError c m x = case x of
  Just x' -> Right x'
  Nothing -> Left $ Error c (ErrorMessage m)

errorToString :: (a -> String) -> Error a -> String
errorToString f (Error c (ErrorMessage m)) = f c ++ " " ++ m

initialProcessEvents :: FilePath -> [(Maybe P.PositionRange, X.Event)] -> Either (Error FilePath) [(FileReference, X.Event)]
initialProcessEvents fp xs
    = (error "Missing prefix BeginDocument" . removePrefixWith snd [X.EventBeginDocument] $ xs)
  >>= (error "Missing suffix EndDocument"   . removeSuffixWith snd [X.EventEndDocument])
  >>= (error "Missing PositionRange"        . traverse (withFileReference fp))
  where error = maybeToError fp


newtype Line = Line { getLine :: Int } deriving (Show, Eq, Ord)
newtype Column = Column { getColumn :: Int } deriving (Show, Eq, Ord)
data LineReference = LineReference Line Column deriving (Show, Eq, Ord)
data FileReference
  = FileReferencePoint FilePath LineReference
  | FileReferenceRange FilePath LineReference LineReference
  deriving (Show, Eq, Ord)

toLineReference :: P.Position -> LineReference
toLineReference (P.Position line column) = LineReference (Line line) (Column column)

toFileReference :: FilePath -> P.PositionRange -> FileReference
toFileReference fp (P.PositionRange start end)
  | start == end = FileReferencePoint fp (toLineReference start)
  | otherwise    = FileReferenceRange fp (toLineReference start) (toLineReference end)


withFileReference :: FilePath -> (Maybe P.PositionRange, X.Event) -> Maybe (FileReference, X.Event)
withFileReference fp = traverseOf _1 (toFileReference fp <$>)


data XNEvent
  = XNBeginElement X.Name [(X.Name, [X.Content])]
  | XNEndElement X.Name
  | XNContent X.Content
  deriving (Show, Eq, Ord)

toXNEvent :: X.Event -> Maybe XNEvent
toXNEvent (X.EventBeginElement n as) = Just $ XNBeginElement n as
toXNEvent (X.EventEndElement n)      = Just $ XNEndElement n
toXNEvent (X.EventContent c)         = Just $ XNContent c
toXNEvent _                          = Nothing


-- toXNEvent :: (FilePath, Maybe P.PositionRange, X.Event) -> Maybe (FilePath, Maybe P.PositionRange, X1_Event)
-- toXNEvent = traverseOf _3 subEvent1






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

getResults :: (a -> Maybe b) -> [a] -> Either [a] [b]
getResults f = resultToEither . foldr (getResultHelper f) (Result [] [])

getResult :: (a -> Maybe b) -> a -> Either a b
getResult f x = case f x of
  Just y  -> Right y
  Nothing -> Left x



-- transform1 :: [(FilePath, Maybe P.PositionRange, X.Event)]
--   -> Either
--     [(FilePath, Maybe P.PositionRange, X.Event)]
--     [(FilePath, Maybe P.PositionRange, X1_Event)]
-- transform1 = getResults transform1a


-- transform2 :: [(FilePath, Maybe P.PositionRange, X1_Event)] -> Maybe [(FilePath, Maybe P.PositionRange, X1_Event)]
-- transform2 = removePrefixWith (^. _3) [X1_BeginDocument]

-- transform3 :: [(FilePath, Maybe P.PositionRange, X1_Event)] -> Maybe [(FilePath, Maybe P.PositionRange, X1_Event)]
-- transform3 = removeSuffixWith (^. _3) [X1_EndDocument]

-- transform4a :: (FilePath, Maybe P.PositionRange, X1_Event) -> Maybe (FilePath, P.PositionRange, X1_Event)
-- transform4a = traverseOf _2 id

-- data X2_Event
--   = X2_BeginElement X.Name [(X.Name, [X.Content])]
--   | X2_EndElement X.Name
--   | X2_Content X.Content
--   deriving (Show, Eq, Ord)

-- subEvent2 :: X1_Event -> Maybe X2_Event
-- subEvent2 (X1_BeginElement n as) = Just $ X2_BeginElement n as
-- subEvent2 (X1_EndElement n)      = Just $ X2_EndElement n
-- subEvent2 (X1_Content c)         = Just $ X2_Content c
-- subEvent2 _                      = Nothing

-- transform5a :: (FilePath, P.PositionRange, X1_Event) -> Maybe (FilePath, P.PositionRange, X2_Event)
-- transform5a = traverseOf _3 subEvent2


-- handle :: Monad m => (a -> m b) -> (a -> Either a b) -> a -> m b
-- handle h f x =
--   case f x of
--     Left  e -> h e
--     Right r -> pure r

-- data EventError
--   = UnexpectedElements [(FilePath, Maybe P.PositionRange, X.Event)]
--   | MissingPrefix [(FilePath, Maybe P.PositionRange, X.Event)]
--   | MissingSuffix [(FilePath, Maybe P.PositionRange, X.Event)]
--   deriving (Show)

-- allTransforms :: [(FilePath, Maybe P.PositionRange, X.Event)] -> Either EventError [(FilePath, Maybe P.PositionRange, X1_Event)]
-- allTransforms
--     = handle (Left . UnexpectedElements) (getResults transform1a)
--   >>= handle (Left . MissingPrefix) transform2
--   -- >>= handle (Left . MissingSuffix) transform3
