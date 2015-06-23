{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Mounce.Quote where

import Data.Data
import Data.Generics (extQ)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec.Pos (newPos)
import Text.ParserCombinators.Parsec
import Text.Greek.Mounce.Parse

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

handleText :: T.Text -> Maybe ExpQ
handleText x = Just $ appE (varE 'T.pack) $ litE $ StringL $ T.unpack x

parseTopLevel :: Data a => CharParser () a -> String -> Q Exp
parseTopLevel p str = do
  l <- location'
  case parse (setPosition l *> (topLevel p)) "" str of
    Right x -> dataToExpQ (const Nothing `extQ` handleText) x
    Left err -> fail (show err)

rules :: QuasiQuoter
rules = QuasiQuoter
  { quoteExp = parseTopLevel euphonyRules
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

nounCategory :: QuasiQuoter
nounCategory = QuasiQuoter
  { quoteExp = parseTopLevel validNounCategoryParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

adjectiveCategory :: QuasiQuoter
adjectiveCategory = QuasiQuoter
  { quoteExp = parseTopLevel validAdjectiveCategoryParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
