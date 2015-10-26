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
, MutableCache(..)

, SomeProbabilityCache
, EventProbabilityCache
, ProbabilityCacheUpdate(..)

, ProbabilityEval(..)

) where

import Event.Probability

import Data.Maybe (fromMaybe)


-----------------------------------------------------------------------------


class (Show key) => Cache cache key v where
    inCache                    :: cache key v -> key -> IO Bool

    lookupProbCache            :: cache key v -> key -> IO (Maybe v)
    findInProbCacheWithDefault :: cache key v -> v -> key -> IO v

    updateProbCache            :: cache key v -> key -> (v -> v) -> IO ()

    findInProbCacheWithDefault cache def = fmap (fromMaybe def) . lookupProbCache cache
--    updateProbCache cache key = fromMaybe err . updateProbCacheSafe cache key
--                            where err = error $ "update error: " ++ show key
--                                             ++ "not found in cache"

class (Cache cache key v) =>
    MutableCache cache key v where
        insertProbInCache :: cache key v -> key -> v -> IO ()
        findOrElseUpdate  :: cache key v -> v -> key -> IO v

        findOrElseUpdate cache def k = do
            mb <- lookupProbCache cache k
            case mb of Just v -> return v
                       _      -> do insertProbInCache cache k def
                                    return def


type SomeProbabilityCache cache key = (Cache cache key Probability)

type EventProbabilityCache cacheP cachePC ev =
        ( MutableCache cacheP  (Event ev) Probability
        , MutableCache cachePC (Event ev, Event ev) Probability )


class (EventProbabilityCache cacheP cachePC ev, Cache cacheC (Event ev) Int) =>

    ProbabilityCacheUpdate cacheP cachePC cacheC ev where

        estimateAndUpdateProb :: cacheP  (Event ev) Probability
                              -> cachePC (Event ev, Event ev) Probability
                              -> cacheC  (Event ev) Int
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
class ( EventProbabilityCache  cacheP cachePC ev
      , ProbabilityCacheUpdate cacheP cachePC cacheC ev ) =>

    ProbabilityEval cacheP cachePC cacheC ev where
        tryEvalProb :: cacheP  (Event ev) Probability
                    -> cachePC (Event ev, Event ev) Probability
                    -> cacheC  (Event ev) Int
                    -> EvProb ev
                    -> IO (EvProb ev)



