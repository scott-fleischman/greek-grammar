module Text.Greek.Mounce.Parse where

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
euphonyRule = (EuphonyRule <$> (greekWord <* spaces <* char '+' <* spaces) <*> (greekWord <* spaces <* char '}' <* spaces) <*> (greekWord <* spaces)) <* spaces

euphonyRules :: CharParser () [EuphonyRule]
euphonyRules = many1 euphonyRule

greekWordsParser :: CharParser () [String]
greekWordsParser = many1 (greekWord <* spaces)

caseEnding :: CharParser () String
caseEnding = string "-" <|> greekWord

nounCaseEndingsParser :: CharParser () (NounForms String)
nounCaseEndingsParser = NounForms
    <$> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)
    <*> (caseEnding <* spaces)

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
