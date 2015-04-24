module Text.Greek.Mounce.Parse where

import Text.ParserCombinators.Parsec
import Text.Greek.Mounce.Euphony
import Text.Greek.Script.UnicodeTokenPairs

greekCharacter :: CharParser () Char
greekCharacter = oneOf $ map fst unicodeTokenPairs

greekWord :: CharParser () String
greekWord = many greekCharacter

euphonyRule :: CharParser () EuphonyRule
euphonyRule = (EuphonyRule <$> (greekWord <* spaces <* char '+' <* spaces) <*> (greekWord <* spaces <* char '}' <* spaces) <*> (greekWord <* spaces)) <* spaces

euphonyRules :: CharParser () [EuphonyRule]
euphonyRules = spaces *> (many1 euphonyRule) <* eof
