{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Phonology.Contractions where

import Control.Lens
import Data.Function
import Data.List
import Data.Map.Strict
import Text.Greek.Phonology.Vowels

data Contraction = Contraction
  { _target :: VowelPhoneme
  , _first :: VowelPhoneme
  , _second :: VowelPhoneme
  }
  deriving (Show)
makeLenses ''Contraction

tryApply :: Contraction -> VowelPhoneme -> VowelPhoneme -> Maybe VowelPhoneme
tryApply (Contraction t p1 p2) v1 v2 = Nothing

groupedContractions = groupBy (\x y -> x ^. first == y ^. first) $ sortedContractions
sortedContractions = sortBy (compare `on` _first) contractions

contractions :: [Contraction]
contractions =
  [ Contraction (alpha Long) (alpha Short) (alpha Short)
  , Contraction (alpha Long) (alpha Long) (alpha Short)
  , Contraction (alpha Long) (alpha Short) (alpha Long)
  , Contraction alphaIota (alpha Short) alphaIota
  , Contraction improperAlpha (alpha Short) improperAlpha
  , Contraction (alpha Long) (alpha Short) epsilon
  , Contraction improperAlpha (alpha Short) epsilonIota
  , Contraction (alpha Long) (alpha Short) spuriousEI
  , Contraction (alpha Long) (alpha Short) eta
  , Contraction improperAlpha (alpha Short) improperEta
  , Contraction alphaIota (alpha Short) (iota Short)
  , Contraction improperAlpha (alpha Long) (iota Short)
  , Contraction omega (alpha Short) omicron
  , Contraction improperOmega (alpha Short) omicronIota
  , Contraction omega (alpha Short) spuriousOU
  , Contraction omega (alpha Short) omega
  , Contraction eta epsilon (alpha Short)
  , Contraction (alpha Long) epsilon (alpha Short)
  , Contraction eta epsilon (alpha Long)
  , Contraction eta eta eta 
  , Contraction improperEta eta improperEta
  , Contraction improperOmega eta omicronIota
  , Contraction improperEta eta (iota Short)
  , Contraction (iota Long) (iota Short) (iota Short)
  , Contraction omega omicron (alpha Short)
  , Contraction (alpha Long) omicron (alpha Short)
  , Contraction eta eta epsilon
  , Contraction improperEta eta epsilonIota
  , Contraction eta eta spuriousEI
  , Contraction omicronIota omicron improperEta
  , Contraction improperOmega omicron improperEta
  , Contraction omicronIota omicron (iota Short)
  , Contraction spuriousOU omicron omicron
  , Contraction omicronIota omicron omicronIota
  , Contraction spuriousOU omicron spuriousOU
  , Contraction omega omicron omega
  , Contraction (upsilon Long) (upsilon Short) (upsilon Short)
  , Contraction omega omega (alpha Short)
  , Contraction improperOmega omega (iota Short)
  , Contraction omega omega omega 
  , Contraction spuriousOU omicron epsilon
  , Contraction omicronIota omicron epsilonIota
  , Contraction omicronUpsilon omicron spuriousEI
  , Contraction omega omicron eta
  , Contraction improperEta epsilon alphaIota
  , Contraction alphaIota epsilon alphaIota
  , Contraction spuriousEI epsilon epsilon
  , Contraction epsilonIota epsilon epsilonIota
  , Contraction spuriousEI epsilon spuriousEI
  , Contraction eta epsilon eta
  , Contraction improperEta epsilon improperEta
  , Contraction epsilonIota epsilon (iota Short)
  , Contraction spuriousOU epsilon omicron
  , Contraction omicronIota epsilon omicronIota
  , Contraction omicronUpsilon epsilon spuriousOU
  , Contraction epsilonUpsilon epsilon (upsilon Short)
  , Contraction omega epsilon omega
  , Contraction improperOmega epsilon improperOmega
  , Contraction improperEta eta alphaIota
  , Contraction omicronUpsilon omicron omicron
  ]

forwardContractionTests :: [(VowelPhoneme, VowelPhoneme, [VowelPhoneme])]
forwardContractionTests =
  [ (alpha Long, omega, [])
  , (omicron, eta, [omega])
  , (alpha Long, alpha Long, [alpha Long, omega])
  ]

inverseContractionTests :: [(VowelPhoneme, [(VowelPhoneme, VowelPhoneme)])]
inverseContractionTests =
  [ (improperOmega, [(epsilon, omicron), (omicron, epsilon)])
  ]
