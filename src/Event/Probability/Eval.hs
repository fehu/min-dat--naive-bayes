{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Event.Probability.Eval
-- Copyright   :  Probability evaluation.
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module Event.Probability.Eval (

  Cache(..)
, SomeProbabilityCache
, EventProbabilityCache

, ProbabilityEval(..)

) where

import Event.Probability

import Data.Maybe (fromMaybe)


-----------------------------------------------------------------------------


class (Show key) => Cache m cache key v where
    inCache                    :: cache m v -> key -> Bool

    lookupProbCache            :: cache m v -> key -> Maybe (m v)
    findInProbCacheWithDefault :: cache m v -> m v -> key -> m v

    updateProbCacheSage        :: cache m v -> key -> (v -> v) -> Maybe (m ())
    updateProbCache            :: cache m v -> key -> (v -> v) -> m ()

    findInProbCacheWithDefault cache def = fromMaybe def . lookupProbCache cache
    updateProbCache cache key = fromMaybe err . updateProbCacheSage cache key
                            where err = error $ "update error: " ++ show key
                                             ++ "not found in cache"

type SomeProbabilityCache m cache key = (Cache m cache key Probability)

type EventProbabilityCache m cacheP cachePC ev =
        ( SomeProbabilityCache m cacheP  (Event ev)
        , SomeProbabilityCache m cachePC (Event ev, Event ev))


-- | Tries to calculate the probability of given event(s) using cache.
class EventProbabilityCache m cacheP cachePC ev =>

    ProbabilityEval m cacheP cachePC ev where
        tryEvalProb :: cacheP m Probability
                    -> cachePC m Probability
                    -> EvProb ev
                    -> m (EvProb ev)



