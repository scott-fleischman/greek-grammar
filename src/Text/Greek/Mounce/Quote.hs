module Text.Greek.Mounce.Quote where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

parseRules :: String -> Q Exp
parseRules s = dataToExpQ (const Nothing) s

rules :: QuasiQuoter
rules = QuasiQuoter
  { quoteExp = parseRules
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
