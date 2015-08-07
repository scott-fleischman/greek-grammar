{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Mounce.Verb where

data Tense = Present | Future | Imperfect | Aorist
data Voice = Active | Middle | Passive
data Mood = Indicative | Subjunctive

data VerbKind a b c d where
  PresentActiveIndicative :: VerbKind 1 'Present 'Active 'Indicative
  PresentMiddleIndicative :: VerbKind 1 'Present 'Middle 'Indicative
  PresentPassiveIndicative :: VerbKind 1 'Present 'Passive 'Indicative
  Aorist2Indicative :: VerbKind 2 'Aorist 'Active 'Indicative
