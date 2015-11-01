{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

-- |
--
-- Module      :  EventProbability.Cache
-- Description :  'Cache's used for 'Event' probabilities.
-- License     :  MIT
--
--


module EventProbability.Cache (

-- * Count Cache
  EventCountCache
, countOccurences
, updateCountCache

-- * 'Prob' Caches
, EventProbCache
, EventCondProbCache

-- * Containers
, EventCaches(..)

) where


import Cache
import EventProbability

import qualified Data.Set as Set
import qualified Data.Map as Map


-----------------------------------------------------------------------------

-- | A cache for counting 'Event's.
type EventCountCache cache m = Cache cache m Event Int

-- | given an event, searches in cache for events,
--   that contain the event in question, and returns thier sum.
countOccurences :: (EventCountCache cache m) =>
    cache Event Int -> Event -> m Int

-- | updates events count (accumulative) in cache.
updateCountCache :: (EventCountCache cache IO) =>
    cache Event Int -> [Event] -> IO (cache Event Int)


countOccurences cache (Event evset) = do
    mcs <- filterCacheByKey cache (\(Event k) -> evset `Map.isSubmapOf` k)
    return . sum $ do
        (_, mc) <- mcs
        return mc

updateCountCache cache events = do sequence_ upds
                                   return cache
    where upds = do ev <- events
                    let f = maybe 1 (1+)
                    return $ updateOrInsert cache f ev

-----------------------------------------------------------------------------

-- | A cache for 'Event's probabilities.
type EventProbCache      cache m = Cache cache m Event Prob
-- | A cache for 'Event's conditional probabilities.
type EventCondProbCache  cache m = Cache cache m (Event, EvAtom) Prob

-----------------------------------------------------------------------------

-- | A container for 'EventProbCache', 'EventProbCache' and 'EventCondProbCache'.
data EventCaches m = forall cc pc cpc . ( EventCountCache     cc m
                                        , EventProbCache      pc m
                                        , EventCondProbCache cpc m ) =>
     EvCaches { countCache    :: cc  Event           Int
              , probCache     :: pc  Event           Prob
              , condProbCache :: cpc (Event, EvAtom) Prob
              }

-----------------------------------------------------------------------------


