module Text.Greek.Mounce.Quote where

import Data.Data
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

parseTopLevel :: Data a => CharParser () a -> String -> Q Exp
parseTopLevel p str = do
  l <- location'
  case parse (setPosition l *> (topLevel p)) "" str of
    Right x -> dataToExpQ (const Nothing) x
    Left err -> fail (show err)

rules :: QuasiQuoter
rules = QuasiQuoter
  { quoteExp = parseTopLevel euphonyRules
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

greekWords :: QuasiQuoter
greekWords = QuasiQuoter
  { quoteExp = parseTopLevel greekWordsParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

nounCaseEndings :: QuasiQuoter
nounCaseEndings = QuasiQuoter
  { quoteExp = parseTopLevel nounCaseEndingsParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
