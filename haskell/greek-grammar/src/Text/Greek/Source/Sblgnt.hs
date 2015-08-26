{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), Word)
import Control.Lens
import Data.Text (Text)
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Data.XML.Types as X
import qualified Text.Greek.Sublist as S


readSblgntEvents :: FilePath -> IO
  ( [SblgntError]
  + [FileReference * (BasicEvent Element9 X.Content XmlAttributes + Word)]
  )
readSblgntEvents = fmap sblgntTransform . readEvents

sblgntTransform
  :: [XmlInternalError] + [FileReference * X.Event]
  ->  [SblgntError]
    + [FileReference * (BasicEvent Element9 X.Content XmlAttributes + Word)]
sblgntTransform x
  =   liftErrors SblgntErrorXmlInternal x
  >>. dropComments
  >>. trimContent _2
  >>= liftErrors SblgntErrorXml . toBasicEvents
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) toElement11 (errorContext SblgntErrorUnexpectedElementName _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) toElement11 (errorContext SblgntErrorUnexpectedElementName _1)

  -- sblgnt
  >>= liftError SblgntErrorSblgntSublist . S.foldrSublist (splitElement splitSblgnt11)
  >>= split . over (each . _Left) SblgntErrorUnexpectedTopLevel
  >>= tryOverAll (_1 . _2) empty (errorContext SblgntErrorInvalidSblgntAttributes (_1 . _1))
  >>. fmap (view _3)
  >>= single SblgntErrorEmptyTopLevel SblgntErrorUnexpectedTopLevels

  -- w
  >>= liftError SblgntErrorSblgntSublist . S.foldrSublist (splitElement splitW10)
  >>= over _Left (fmap SblgntErrorWord . concat) . tryOverAll _Right createWord seq
  >>. fmap extractProduct

data SblgntError
  = SblgntErrorXmlInternal XmlInternalError
  | SblgntErrorXml (XmlError FileReference)
  | SblgntErrorUnexpectedNamespace FileReference X.Name
  | SblgntErrorUnexpectedElementName FileReference XmlLocalName
  | SblgntErrorSblgntSublist (S.Error (FileReference * XmlAttributes) FileReference)
  | SblgntErrorUnexpectedTopLevel (FileReference * BasicEvent Element10 X.Content XmlAttributes)
  | SblgntErrorInvalidSblgntAttributes FileReference XmlAttributes
  | SblgntErrorEmptyTopLevel
  | SblgntErrorUnexpectedTopLevels [FileReference * BasicEvent Element10 X.Content XmlAttributes]
  | SblgntErrorWord WordError
  deriving (Show)

data Element11
  = Element11Sblgnt
  | Element11A
  | Element11Book
  | Element11License
  | Element11MarkEnd
  | Element11P
  | Element11Prefix
  | Element11Suffix
  | Element11Title
  | Element11VerseNumber
  | Element11W
  deriving (Eq, Ord, Show)

toElement11 :: XmlLocalName -> XmlLocalName + Element11
toElement11 (XmlLocalName "sblgnt"      ) = Right Element11Sblgnt
toElement11 (XmlLocalName "a"           ) = Right Element11A
toElement11 (XmlLocalName "book"        ) = Right Element11Book
toElement11 (XmlLocalName "license"     ) = Right Element11License
toElement11 (XmlLocalName "mark-end"    ) = Right Element11MarkEnd
toElement11 (XmlLocalName "p"           ) = Right Element11P
toElement11 (XmlLocalName "prefix"      ) = Right Element11Prefix
toElement11 (XmlLocalName "suffix"      ) = Right Element11Suffix
toElement11 (XmlLocalName "title"       ) = Right Element11Title
toElement11 (XmlLocalName "verse-number") = Right Element11VerseNumber
toElement11 (XmlLocalName "w"           ) = Right Element11W
toElement11 t                             = Left t

data Element10
  = Element10A
  | Element10Book
  | Element10License
  | Element10MarkEnd
  | Element10P
  | Element10Prefix
  | Element10Suffix
  | Element10Title
  | Element10VerseNumber
  | Element10W
  deriving (Eq, Ord, Show)

splitSblgnt11 :: Element11 -> () + Element10
splitSblgnt11 Element11Sblgnt      = Left ()
splitSblgnt11 Element11A           = Right Element10A
splitSblgnt11 Element11Book        = Right Element10Book
splitSblgnt11 Element11License     = Right Element10License
splitSblgnt11 Element11MarkEnd     = Right Element10MarkEnd
splitSblgnt11 Element11P           = Right Element10P
splitSblgnt11 Element11Prefix      = Right Element10Prefix
splitSblgnt11 Element11Suffix      = Right Element10Suffix
splitSblgnt11 Element11Title       = Right Element10Title
splitSblgnt11 Element11VerseNumber = Right Element10VerseNumber
splitSblgnt11 Element11W           = Right Element10W

data Element9
  = Element9A
  | Element9Book
  | Element9License
  | Element9MarkEnd
  | Element9P
  | Element9Prefix
  | Element9Suffix
  | Element9Title
  | Element9VerseNumber
  deriving (Eq, Ord, Show)

splitW10 :: Element10 -> () + Element9
splitW10 Element10W           = Left ()
splitW10 Element10A           = Right Element9A
splitW10 Element10Book        = Right Element9Book
splitW10 Element10License     = Right Element9License
splitW10 Element10MarkEnd     = Right Element9MarkEnd
splitW10 Element10P           = Right Element9P
splitW10 Element10Prefix      = Right Element9Prefix
splitW10 Element10Suffix      = Right Element9Suffix
splitW10 Element10Title       = Right Element9Title
splitW10 Element10VerseNumber = Right Element9VerseNumber

splitElement
  :: (e -> a + e')
  -> (FileReference * BasicEvent e X.Content XmlAttributes)
  -> S.TopLevel
    (FileReference * XmlAttributes)
    (FileReference)
    (FileReference * BasicEvent e' X.Content XmlAttributes)
splitElement f (r, BasicEventBeginElement e a) = case f e of
  Left _   -> S.TopLevelBegin (r, a)
  Right e' -> S.TopLevelPass (r, BasicEventBeginElement e' a)
splitElement f (r, BasicEventEndElement e) = case f e of
  Left _   -> S.TopLevelEnd r
  Right e' -> S.TopLevelPass (r, BasicEventEndElement e')
splitElement _ (r, BasicEventContent c)
            = S.TopLevelPass (r, BasicEventContent c)

newtype GreekText = GreekText { getGreekText :: Text } deriving (Eq, Ord, Show)
data Word = Word
  { wordGreekText :: GreekText
  } deriving (Show)

data WordError
  = WordErrorInvalidAttributes FileReference XmlAttributes
  | WordErrorEmptyContent FileReference
  | WordErrorInvalidContent (FileReference * BasicEvent Element9 X.Content XmlAttributes)
  deriving (Show)

createWord
  :: (FileReference * XmlAttributes,
      FileReference,
      [FileReference * BasicEvent Element9 X.Content XmlAttributes])
  -> [WordError] + FileReference * Word
createWord ((r, a@(_ : _)), _, _)                                          = Left . pure $ WordErrorInvalidAttributes r a
createWord ((r, []),        _, [])                                         = Left . pure $ WordErrorEmptyContent r
createWord ((_, []),        _, c@(_ : _ : _))                              = Left $ fmap WordErrorInvalidContent c
createWord ((_, []),        _, [(r, BasicEventContent (X.ContentText t))]) = Right $ (r, Word (GreekText t))
createWord ((_, []),        _, [c])                                        = Left . pure $ WordErrorInvalidContent c
