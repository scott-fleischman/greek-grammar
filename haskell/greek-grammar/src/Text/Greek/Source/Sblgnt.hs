{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), log, FilePath)
import Control.Lens
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Prelude as X (FilePath)

type FinalXmlEvent
  = XmlBeginElement * ElementAll * XmlAttributes
  + XmlEndElement * ElementAll
  + XmlContent

newtype AElement = AElement () deriving (Eq, Ord, Show)
newtype BookElement = BookElement () deriving (Eq, Ord, Show)
newtype LicenseElement = LicenseElement () deriving (Eq, Ord, Show)
newtype MarkEndElement = MarkEndElement () deriving (Eq, Ord, Show)
newtype PElement = PElement () deriving (Eq, Ord, Show)
newtype PrefixElement = PrefixElement () deriving (Eq, Ord, Show)
newtype SblgntElement = SblgntElement () deriving (Eq, Ord, Show)
newtype SuffixElement = SuffixElement () deriving (Eq, Ord, Show)
newtype TitleElement = TitleElement () deriving (Eq, Ord, Show)
newtype VerseNumberElement = VerseNumberElement () deriving (Eq, Ord, Show)
newtype WElement = WElement () deriving (Eq, Ord, Show)

type ElementAll
  = AElement
  + BookElement
  + LicenseElement
  + MarkEndElement
  + PElement
  + PrefixElement
  + SblgntElement
  + SuffixElement
  + TitleElement
  + VerseNumberElement
  + WElement

toElementAll :: XmlLocalName -> XmlLocalName + ElementAll
toElementAll (XmlLocalName "a"           ) = Right . sum1   $ AElement ()
toElementAll (XmlLocalName "book"        ) = Right . sum2   $ BookElement ()
toElementAll (XmlLocalName "license"     ) = Right . sum3   $ LicenseElement ()
toElementAll (XmlLocalName "mark-end"    ) = Right . sum4   $ MarkEndElement ()
toElementAll (XmlLocalName "p"           ) = Right . sum5   $ PElement ()
toElementAll (XmlLocalName "prefix"      ) = Right . sum6   $ PrefixElement ()
toElementAll (XmlLocalName "sblgnt"      ) = Right . sum7   $ SblgntElement ()
toElementAll (XmlLocalName "suffix"      ) = Right . sum8   $ SuffixElement ()
toElementAll (XmlLocalName "title"       ) = Right . sum9   $ TitleElement ()
toElementAll (XmlLocalName "verse-number") = Right . sum10  $ VerseNumberElement ()
toElementAll (XmlLocalName "w"           ) = Right . sum11e $ WElement ()
toElementAll t                             = Left t



readSblgntEvents :: X.FilePath -> IO ([ErrorMessage] + [FileReference * FinalXmlEvent])
readSblgntEvents = fmap (>>= tx) . readEvents

tx :: [FileReference * XmlEventAll]
   -> [ErrorMessage] + [FileReference * FinalXmlEvent]
tx x = return x
  >>. trimContent
  >>= removeUnusedXmlEvents
  >>. tx16a
  >>= tx16b
  >>. tx17a
  >>= tx17b
  >>= useBeginElementType
  >>= useEndElementType

type XmlUnused
  = XmlCDATA
  + XmlBeginDoctype * XmlDoctypeName * (None + XmlExternalId)
  + XmlEndDoctype
  + XmlInstruction
  + XmlComment

removeUnusedXmlEvents
  :: Handler e (a * (b + c + d + XmlUnused)) =>
               [a * (b + c + d + XmlUnused)]
  -> [e] +     [a * (b + c + d            )]
removeUnusedXmlEvents = handleMap lens2e tryDrop4e

tx16a :: [a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix)) * y + b2)]
      -> [a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix))          * y + b2)]
tx16a = over (each . lens2e . prism1 . lens2 . lens2e) extractSum

tx16b :: Handler e (a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix)) * y + b2)) =>
                   [a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix)) * y + b2)]
      -> [e] +     [a * (XmlBeginElement *  XmlLocalName                                          * y + b2)]
tx16b = handleMap (lens2e . prism1 . lens2) tryDrop2eNone

tx17a :: [a * (b1 + XmlEndElement * (XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix)) + b2)]
      -> [a * (b1 + XmlEndElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix))          + b2)]
tx17a = over (each . lens2e . prism2 . lens2e . lens2e) extractSum

tx17b :: Handler e (a * (b1 + XmlEndElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix)) + b2)) =>
                   [a * (b1 + XmlEndElement * (XmlLocalName * (None + XmlNamespace * XmlNamePrefix)) + b2)]
      -> [e] +     [a * (b1 + XmlEndElement *  XmlLocalName                                          + b2)]
tx17b = handleMap (lens2e . prism2 . lens2e) tryDrop2eNone

useBeginElementType
  :: Handler e (a * (XmlBeginElement * XmlLocalName * as + b2)) =>
               [a * (XmlBeginElement * XmlLocalName * as + b2)]
  -> [e] +     [a * (XmlBeginElement * ElementAll   * as + b2)]
useBeginElementType = handleMap' (lens2e . prism1 . lens2) toElementAll

useEndElementType
  :: Handler e (a * (b1 + XmlEndElement * XmlLocalName + b3)) =>
               [a * (b1 + XmlEndElement * XmlLocalName + b3)]
  -> [e] +     [a * (b1 + XmlEndElement * ElementAll   + b3)]
useEndElementType = handleMap' (lens2e . prism2 . lens2e) toElementAll
