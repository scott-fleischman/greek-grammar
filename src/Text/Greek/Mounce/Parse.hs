module Text.Greek.Mounce.Parse where

import Data.Text (pack)
import Text.ParserCombinators.Parsec
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Phonology
import Text.Greek.Script.UnicodeTokenPairs

greekCharacter :: CharParser () Char
greekCharacter = oneOf $ greekDasia : map fst unicodeTokenPairs
  where greekDasia = '\x1FFE'

greekWord :: CharParser () String
greekWord = many1 greekCharacter

euphonyRule :: CharParser () EuphonyRule
euphonyRule = EuphonyRule
  <$> (pack <$> greekWord <* spaces <* char '+' <* spaces)
  <*> (pack <$> greekWord <* spaces <* char '}' <* spaces)
  <*> (pack <$> greekWord <* spaces)

euphonyRules :: CharParser () [EuphonyRule]
euphonyRules = sepBy1 euphonyRule spaces

greekWordsParser :: CharParser () [String]
greekWordsParser = endBy1 greekWord spaces

caseEnding :: CharParser () Affix
caseEnding = pure EmptyAffix <* string "-"
  <|> pure UnattestedAffix <* string "*"
  <|> AttestedAffix <$> greekWord

nounCaseEndingsParser :: CharParser () (NounForms Affix)
nounCaseEndingsParser = NounForms <$> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e
  where e = caseEnding <* spaces

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
