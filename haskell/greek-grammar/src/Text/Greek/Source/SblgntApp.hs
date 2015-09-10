{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.SblgntApp where

import Data.Text (Text)
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import qualified Data.Text as T

data AppBook = AppBook
  { appBookTitle :: Text
  , appBookVariants :: [Variant]
  } deriving Show

data Variant = Variant
  { variantVerse :: Text
  , variantText :: Text
  } deriving Show

verseNumberParser :: EventParser Text
verseNumberParser = element "verse-number" (simpleAttributeParser "id") contentParser const

boldParser :: EventParser Text
boldParser = elementContent "b"

variantTextParser :: EventParser Text
variantTextParser = do
  text1 <- boldParser
  text2 <- contentParser
  text3 <- boldParser
  text4 <- contentParser
  return $ T.concat [text1, text2, text3, text4]

variantParser :: EventParser Variant
variantParser = elementSimple "p" body
  where
    body = do
      verse <- verseNumberParser
      text <- variantTextParser
      return $ Variant verse text

appBookParser :: EventParser AppBook
appBookParser = elementSimple "book" body
  where
    body = do
      title <- elementContent "title"
      variants <- many1 variantParser
      return $ AppBook title variants

sblgntAppParser :: EventParser [AppBook]
sblgntAppParser = elementSimple "sblgntapp" (many1 appBookParser)
