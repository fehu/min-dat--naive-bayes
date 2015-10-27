{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Event.Probability.Eval
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Event.Probability.Eval (

  ProbabilityEval(..)

) where

import Event.Probability
import Event.Probability.Cache


-- | Tries to calculate the probability of given event(s) using cache.
class ( EventProbabilityCache  cacheP cachePC ev
      , ProbabilityCacheUpdate cacheP cachePC cacheC ev ) =>

    ProbabilityEval cacheP cachePC cacheC ev where
        tryEvalProb :: cacheP  (Event ev) Probability
                    -> cachePC (Event ev, Event ev) Probability
                    -> cacheC  (Event ev) Int
                    -> EvProb ev
                    -> IO (EvProb ev)




