{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  Event.Probability.Cache
-- Copyright   :  Probability evaluation.
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module Event.Probability.Cache (

  Cache(..)
, MutableCache(..)

, SomeProbabilityCache
, EventProbabilityCache

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






