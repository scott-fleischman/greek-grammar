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

caseEndings8 :: CharParser () [String]
caseEndings8 = spaces *> (count 8 (caseEnding <* spaces)) <* eof

wordsToEndings :: [String] -> NounCaseEndings
wordsToEndings ws = NounCaseEndings (ws !! 0) (ws !! 1) (ws !! 2) (ws !! 3) (ws !! 4) (ws !! 5) (ws !! 6) (ws !! 7)

nounCaseEndingsParser :: CharParser () NounCaseEndings
nounCaseEndingsParser = wordsToEndings <$> caseEndings8

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
