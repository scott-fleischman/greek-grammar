module Text.Greek.Mounce.Quote where

import Control.Exception (throwIO)
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

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a

parseTopLevel :: Data a => CharParser () a -> String -> Q Exp
parseTopLevel p str = do
  l <- location'
  c <- runIO $ parseIO (setPosition l *> (topLevel p)) str
  dataToExpQ (const Nothing) c

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
