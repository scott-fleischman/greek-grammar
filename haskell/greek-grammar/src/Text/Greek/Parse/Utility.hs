{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.Parse.Utility where

import Prelude hiding (getLine)
import Control.Lens
import Text.Greek.FileReference
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Pos as P

type Parser s t = ParsecT s () Identity t

nestParser :: (Stream s1 m t1, Stream s2 Identity t2) => ParsecT s1 u m s2 -> ParsecT s2 () Identity b -> ParsecT s1 u m b
nestParser p1 p2 = p1 >>= embedParser p2

embedParser :: (Stream s1 m t1, Stream s2 Identity t2) => ParsecT s2 () Identity b -> s2 -> ParsecT s1 u m b
embedParser p2 v = do
  pos <- getPosition
  case parse (setPosition pos *> p2) (P.sourceName pos) v of
    Left x -> fail $ show x
    Right x -> return x

primMaybe :: (Show t, Stream s m t) => (t -> LineReference) -> (t -> Maybe a) -> ParsecT s u m a
primMaybe f = tokenPrim show (updateEventPos f)

primBool :: (Show t, Stream s m t) => (t -> LineReference) -> (t -> Bool) -> ParsecT s u m t
primBool f g = primMaybe f go
  where
    go t | g t = Just t
    go _ = Nothing

updateEventPos :: (t -> LineReference) -> P.SourcePos -> t -> s -> P.SourcePos
updateEventPos f p r _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    line   = (f r) ^. lineReferenceLine   . getLine
    column = (f r) ^. lineReferenceColumn . getColumn

tryManyEnd :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
tryManyEnd b e = tryEnd []
  where
    more xs = do
      x <- b
      tryEnd (x : xs)
    tryEnd xs = try (do { x <- e; return (reverse (x : xs)) })
      <|> more xs

tryManyEndEof :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
tryManyEndEof b e = tryManyEnd b (e <* eof)
