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

makeError :: a -> String -> Error a
makeError c m = Error c (ErrorMessage m)

maybeToError :: c -> String -> Maybe a -> Either (Error c) a
maybeToError c m x = case x of
  Just x' -> Right x'
  Nothing -> Left . makeError c $ m

errorToString :: (a -> String) -> Error a -> String
errorToString f (Error c (ErrorMessage m)) = f c ++ " " ++ m

initialProcessEvents :: FilePath -> [(Maybe P.PositionRange, X.Event)] -> Either (Error FilePath) [(FileReference, X.Event)]
initialProcessEvents fp xs
    = (e "Missing prefix BeginDocument" . removePrefixWith snd [X.EventBeginDocument] $ xs)
  >>= (e "Missing suffix EndDocument"   . removeSuffixWith snd [X.EventEndDocument])
  >>= (e "Missing PositionRange"        . traverse (withFileReference fp))
  where e = maybeToError fp


newtype Line = Line { getLine :: Int } deriving (Show, Eq, Ord)
newtype Column = Column { getColumn :: Int } deriving (Show, Eq, Ord)
data LineReference = LineReference Line Column deriving (Show, Eq, Ord)
data FileReference
  = FileReferencePoint FilePath LineReference
  | FileReferenceRange FilePath LineReference LineReference
  deriving (Show, Eq, Ord)

lineReferenceToString :: LineReference -> String
lineReferenceToString (LineReference (Line line) (Column column)) = show line ++ ":" ++ show column

fileReferenceToString :: FileReference -> String
fileReferenceToString (FileReferencePoint p r) = show p ++ ":" ++ lineReferenceToString r
fileReferenceToString (FileReferenceRange p r1 r2) = show p ++ ":" ++ lineReferenceToString r1 ++ "-" ++ lineReferenceToString r2

toLineReference :: P.Position -> LineReference
toLineReference (P.Position line column) = LineReference (Line line) (Column column)

toFileReference :: FilePath -> P.PositionRange -> FileReference
toFileReference fp (P.PositionRange start end)
  | start == end = FileReferencePoint fp (toLineReference start)
  | otherwise    = FileReferenceRange fp (toLineReference start) (toLineReference end)


withFileReference :: FilePath -> (Maybe P.PositionRange, X.Event) -> Maybe (FileReference, X.Event)
withFileReference fp = traverseOf _1 (toFileReference fp <$>)


data XNCEvent
  = XNCBeginElement X.Name [(X.Name, [X.Content])]
  | XNCEndElement X.Name
  | XNCContent X.Content
  deriving (Show)

toXNCEvent :: X.Event -> Maybe XNCEvent
toXNCEvent (X.EventBeginElement n as) = Just $ XNCBeginElement n as
toXNCEvent (X.EventEndElement n)      = Just $ XNCEndElement n
toXNCEvent (X.EventContent c)         = Just $ XNCContent c
toXNCEvent _                          = Nothing


toXNCEventContext :: (FileReference, X.Event) -> Maybe (FileReference, XNCEvent)
toXNCEventContext = traverseOf _2 toXNCEvent

tryMap :: Either [Error x] a -> Maybe b -> Error x -> (b -> a -> c) -> Either [Error x] c
tryMap (Left  es) Nothing  e _ = Left  (e : es)
tryMap (Left  es) (Just _) _ _ = Left  es
tryMap (Right _ ) Nothing  e _ = Left  [e]
tryMap (Right a ) (Just b) _ f = Right (f b a)

tryConvertAll :: (a -> Error c) -> (a -> Maybe b) -> [a] -> Either [Error c] [b]
tryConvertAll e f xs = foldr (\a b -> tryMap b (f a) (e a) (:)) (Right []) xs

showError :: Show b => String -> (a, b) -> Error a
showError m (x, y) = Error x (ErrorMessage (m ++ " " ++ show y))

toXNCEventsError :: [(FileReference, X.Event)] -> Either [Error FileReference] [(FileReference, XNCEvent)]
toXNCEventsError = tryConvertAll (showError "Unexpected event") toXNCEventContext


newtype ElementName = ElementName { getElementName :: Text }
newtype AttributeName = AttributeName { getAttributeName :: Text }
data XCEvent
  = XCBeginElement ElementName [(AttributeName, [X.Content])]
  | XCEndElement ElementName
  | XCContent X.Content

nameToText :: X.Name -> Maybe Text
nameToText (X.Name t Nothing Nothing) = Just t
nameToText _ = Nothing


toAttributeName :: (X.Name, a) -> Maybe (AttributeName, a)
toAttributeName = traverseOf _1 (fmap AttributeName . nameToText)

toAttributeNameError :: [(X.Name, a)] -> Either [Error ()] [(AttributeName, a)]
toAttributeNameError = tryConvertAll (showError "Unexpected attribute namespace" . ((,) ()) . fst) toAttributeName

toElementName :: X.Name -> Maybe ElementName
toElementName = fmap ElementName . nameToText

ensureNoNamespaces :: XNCEvent -> Either [Error ()] XCEvent
ensureNoNamespaces (XNCBeginElement n as) = tryMap (toAttributeNameError as) (ElementName <$> nameToText n) (makeError () ("Unexpected element namespace" ++ show n)) XCBeginElement
ensureNoNamespaces (XNCEndElement n) = fmap XCEndElement (_Left %~ pure $ (maybeToError () ("Unexpected element namespace " ++ show n) (ElementName <$> nameToText n)))
ensureNoNamespaces (XNCContent c) = pure (XCContent c)

ensureNoNamespacesContext :: (FileReference, XNCEvent) -> Either [Error FileReference] (FileReference, XCEvent)
ensureNoNamespacesContext (x, y) = (_Left %~ (fmap (errorContext .~ x))) . (_Right %~ ((,) x)) $ (ensureNoNamespaces y)

combineEitherList :: Either [a] b -> Either [a] [b] -> Either [a] [b]
combineEitherList (Left as)   (Left  as') = Left (as ++ as')
combineEitherList (Left as)   (Right _  ) = Left as
combineEitherList (Right _) e@(Left  _  ) = e
combineEitherList (Right b)   (Right bs ) = Right (b : bs)

ensureNoNamespacesAll :: [(FileReference, XNCEvent)] -> Either [Error FileReference] [(FileReference, XCEvent)]
ensureNoNamespacesAll = foldr combineEitherList (Right []) . fmap ensureNoNamespacesContext



type Item context item = (context, item)
type MaybeItem error context item = Either (context, error) (context, item)
type Items context item = [(context, item)]
type MaybeItems error context item = Either [(context, error)] [(context, item)]

mapItem :: (i -> i') -> Item c i -> Item c i'
mapItem = fmap

maybeMapItem :: (i -> Either e i') -> Item c i -> MaybeItem e c i'
maybeMapItem f x@(c, i) = _Left %~ ((,) c) $ traverse f x

mapItems :: (i -> i') -> Items c i -> Items c i'
mapItems = fmap . mapItem

maybeMapItems :: (i -> Either e i') -> Items c i -> MaybeItems e c i'
maybeMapItems = _
