{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Text.Greek.Mounce.Verb where

data Tense = Present | Future | Imperfect | Aorist | FuturePerfect | Perfect | Pluperfect
data Voice = Active | Middle | Passive
data Mood = Indicative | Subjunctive | Optative | Imperative | Infinitive 

data VerbKind a b c d where
  PresentActiveIndicative        :: VerbKind 1 'Present       'Active  'Indicative
  PresentActiveSubjunctive       :: VerbKind 1 'Present       'Active  'Subjunctive
  PresentActiveOptative          :: VerbKind 1 'Present       'Active  'Optative
  PresentActiveImperative        :: VerbKind 1 'Present       'Active  'Imperative
  PresentActiveInfinitive        :: VerbKind 1 'Present       'Active  'Infinitive
  PresentMiddleIndicative        :: VerbKind 1 'Present       'Middle  'Indicative
  PresentMiddleSubjunctive       :: VerbKind 1 'Present       'Middle  'Subjunctive
  PresentMiddleOptative          :: VerbKind 1 'Present       'Middle  'Optative
  PresentMiddleInfinitive        :: VerbKind 1 'Present       'Middle  'Infinitive
  PresentMiddleImperative        :: VerbKind 1 'Present       'Middle  'Imperative
  PresentPassiveIndicative       :: VerbKind 1 'Present       'Passive 'Indicative
  PresentPassiveSubjunctive      :: VerbKind 1 'Present       'Passive 'Subjunctive
  PresentPassiveImperative       :: VerbKind 1 'Present       'Passive 'Imperative
  PresentPassiveInfinitive       :: VerbKind 1 'Present       'Passive 'Infinitive
  Aorist1ActiveIndicative        :: VerbKind 1 'Aorist        'Active  'Indicative
  Aorist1ActiveSubjunctive       :: VerbKind 1 'Aorist        'Active  'Subjunctive
  Aorist1ActiveOptative          :: VerbKind 1 'Aorist        'Active  'Optative
  Aorist1ActiveImperative        :: VerbKind 1 'Aorist        'Active  'Imperative
  Aoirst1ActiveInfinitive        :: VerbKind 1 'Aorist        'Active  'Infinitive
  Aorist1MiddleIndicative        :: VerbKind 1 'Aorist        'Middle  'Indicative
  Aorist1MiddleSubjunctive       :: VerbKind 1 'Aorist        'Middle  'Subjunctive
  Aorist1MiddleOptative          :: VerbKind 1 'Aorist        'Middle  'Optative
  Aorist1MiddleImperative        :: VerbKind 1 'Aorist        'Middle  'Optative
  Aorist1MiddleInfinitive        :: VerbKind 1 'Aorist        'Middle  'Infinitive
  Aorist1PassiveIndicative       :: VerbKind 1 'Aorist        'Passive 'Indicative
  Aorist1PassiveSubjunctive      :: VerbKind 1 'Aorist        'Passive 'Subjunctive
  Aorist1PassiveImperative       :: VerbKind 1 'Aorist        'Passive 'Imperative
  Aorist1PassiveInfinitive       :: VerbKind 1 'Aorist        'Passive 'Infinitive
  Aorist2ActiveIndicative        :: VerbKind 2 'Aorist        'Active  'Indicative
  Aorist2ActiveSubjunctive       :: VerbKind 2 'Aorist        'Active  'Indicative
  Aorist2ActiveOptative          :: VerbKind 2 'Aorist        'Active  'Optative
  Aorist2ActiveInfinitive        :: VerbKind 2 'Aorist        'Active  'Infinitive
  Aorist2ActiveImperative        :: VerbKind 2 'Aorist        'Active  'Imperative
  Aorist2MiddleIndicative        :: VerbKind 2 'Aorist        'Middle  'Indicative
  Aorist2MiddleSubjunctive       :: VerbKind 2 'Aorist        'Middle  'Subjunctive
  Aorist2MiddleOptative          :: VerbKind 2 'Aorist        'Middle  'Optative
  Aorist2MiddleImperative        :: VerbKind 2 'Aorist        'Middle  'Imperative
  Aorist2MiddleInfinitive        :: VerbKind 2 'Aorist        'Middle  'Infinitive
  Aorist2PassiveImperative       :: VerbKind 2 'Aorist        'Passive 'Imperative
  Aorist2PassiveInfinitive       :: VerbKind 2 'Aorist        'Passive 'Infinitive
  ImperfectActiveIndicative      :: VerbKind 1 'Imperfect     'Active  'Indicative 
  ImperfectMiddleIndicative      :: VerbKind 1 'Imperfect     'Active  'Indicative 
  ImperfectPassiveIndicative     :: VerbKind 1 'Imperfect     'Passive 'Indicative
  FutureActiveIndicative         :: VerbKind 1 'Future        'Active  'Indicative
  FutureActiveOptative           :: VerbKind 1 'Future        'Active  'Optative
  FutureActiveInfinitive         :: VerbKind 1 'Future        'Active  'Infinitive
  FutureMiddleIndicative         :: VerbKind 1 'Future        'Middle  'Indicative
  FutureMiddleOptative           :: VerbKind 1 'Future        'Middle  'Optative
  FutureMiddleInfinitive         :: VerbKind 1 'Future        'Middle  'Infinitive
  Future1PassiveIndicative       :: VerbKind 1 'Future        'Passive 'Indicative
  Future2PassiveIndicative       :: VerbKind 2 'Future        'Passive 'Indicative
  Perfect1ActiveIndicative       :: VerbKind 1 'Perfect       'Active  'Indicative
  Perfect2ActiveIndicative       :: VerbKind 2 'Perfect       'Active  'Indicative
  PerfectActiveSubjunctive       :: VerbKind 1 'Perfect       'Active  'Subjunctive
  PerfectActiveInfinitive        :: VerbKind 1 'Perfect       'Active  'Infinitive
  PerfectActiveOptative          :: VerbKind 1 'Perfect       'Active  'Optative
  PerfectActiveImperative        :: VerbKind 1 'Perfect       'Active  'Imperative 
  PerfectMiddleIndicative        :: VerbKind 1 'Perfect       'Middle  'Indicative
  PerfectMiddleSubjunctive       :: VerbKind 1 'Perfect       'Middle  'Subjunctive
  PerfectMiddleOptative          :: VerbKind 1 'Perfect       'Middle  'Optative
  PerfectMiddleInfinitive        :: VerbKind 1 'Perfect       'Middle  'Infinitive
  PerfectMiddleImperative        :: VerbKind 1 'Perfect       'Middle  'Imperative
  PerfectPassiveIndicative       :: VerbKind 1 'Perfect       'Passive 'Indicative
  PerfectPassiveSubjunctive      :: VerbKind 1 'Perfect       'Passive 'Subjunctive
  PerfectPassiveInfinitive       :: VerbKind 1 'Perfect       'Passive 'Infinitive
  PerfectPassiveOptative         :: VerbKind 1 'Perfect       'Passive 'Optative
  PerfectPassiveImperative       :: VerbKind 1 'Perfect       'Passive 'Imperative
  FuturePerfectActiveIndicative  :: VerbKind 1 'FuturePerfect 'Active  'Indicative
  FuturePerfectMiddleIndicative  :: VerbKind 1 'FuturePerfect 'Middle  'Indicative
  FuturePerfectPassiveIndicative :: VerbKind 1 'FuturePerfect 'Passive 'Indicative
  PluperfectActiveIndicative     :: VerbKind 1 'Pluperfect    'Active  'Indicative
  PluperfectMiddleIndicative     :: VerbKind 1 'Pluperfect    'Middle  'Indicative
  PluperfectPassiveIndicative    :: VerbKind 1 'Pluperfect    'Passive 'Indicative
