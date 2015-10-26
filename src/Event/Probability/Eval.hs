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
, ProbabilityCacheUpdate(..)

, ProbabilityEval(..)

) where

import Event.Probability

import Data.Maybe (fromMaybe)


-----------------------------------------------------------------------------


class (Show key) => Cache m cache key v where
    inCache                    :: cache m v -> key -> Bool

    lookupProbCache            :: cache m v -> key -> Maybe (m v)
    findInProbCacheWithDefault :: cache m v -> m v -> key -> m v

    updateProbCacheSafe        :: cache m v -> key -> (v -> v) -> Maybe (m ())
    updateProbCache            :: cache m v -> key -> (v -> v) -> m ()

    findInProbCacheWithDefault cache def = fromMaybe def . lookupProbCache cache
    updateProbCache cache key = fromMaybe err . updateProbCacheSafe cache key
                            where err = error $ "update error: " ++ show key
                                             ++ "not found in cache"

class (Monad m, Cache m cache key v) =>
    MutableCache m cache key v where
        insertProbInCache :: cache m v -> key -> v -> m ()
        findOrElseUpdate  :: cache m v -> v -> key -> m v

        findOrElseUpdate cache def k =
            case lookupProbCache cache k of Just v -> v
                                            _      -> do insertProbInCache cache k def
                                                         return def


type SomeProbabilityCache m cache key = (Cache m cache key Probability)

type EventProbabilityCache m cacheP cachePC ev =
        ( MutableCache m cacheP  (Event ev) Probability
        , MutableCache m cachePC (Event ev, Event ev) Probability )


class (EventProbabilityCache m cacheP cachePC ev, Cache m cacheC (Event ev) Int) =>

    ProbabilityCacheUpdate m cacheP cachePC cacheC ev where

        estimateAndUpdateProb :: cacheP m Probability
                              -> cachePC m Probability
                              -> cacheC m Int
                              -> EvProb ev
                              -> Probability
--        estimatePAndUpdateP   :: cacheP m Probability
--                              -> cacheC m Int
--                              -> P ev
--                              -> Probability
--        estimatePAndUpdatePC  :: cachePC m Probability
--                              -> cacheC m Int
--                              -> PCond ev
--                              -> Probability

--        estimateAndUpdateProb cP cPC cC (EvProb ev) =
--            case toEither ev of Left p   -> estimatePAndUpdateP  cP  cC p
--                                Right pc -> estimatePAndUpdatePC cPC cC pc


-- | Tries to calculate the probability of given event(s) using cache.
class ( EventProbabilityCache  m cacheP cachePC ev
      , ProbabilityCacheUpdate m cacheP cachePC cacheC ev ) =>

    ProbabilityEval m cacheP cachePC cacheC ev where
        tryEvalProb :: cacheP  m Probability
                    -> cachePC m Probability
                    -> cacheC  m Int
                    -> EvProb ev
                    -> m (EvProb ev)



