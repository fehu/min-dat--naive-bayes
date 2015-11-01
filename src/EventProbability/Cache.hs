{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

-- |
--
-- Module      :  EventProbability.Cache
-- Description :
-- License     :  MIT
--
--


module EventProbability.Cache (

  EventCountCache
, countOccurences

, EventProbCache
, EventCondProbCache

, EventCaches(..)

) where


import Cache
import EventProbability

import qualified Data.Set as Set


-----------------------------------------------------------------------------

type EventCountCache cache m = Cache cache m Event Int

-- | given an event, searches in cache for events, that contain the event in question,
--   and returns thier sum.
countOccurences :: (EventCountCache cache m) => cache Event Int -> Event -> m Int

countOccurences cache (Event evset) = do
    mcs <- filterCacheByKey cache (\(Event k) -> evset `Set.isSubsetOf` k)
    return . sum $ do
        (_, mc) <- mcs
        return mc


-----------------------------------------------------------------------------

type EventProbCache      cache m = Cache cache m Event Prob
type EventCondProbCache  cache m = Cache cache m (Event, EvAtom) Prob

-----------------------------------------------------------------------------

data EventCaches m = forall cc pc cpc . ( EventCountCache     cc m
                                        , EventProbCache      pc m
                                        , EventCondProbCache cpc m ) =>
     EvCaches { countCache    :: cc  Event           Int
              , probCache     :: pc  Event           Prob
              , condProbCache :: cpc (Event, EvAtom) Prob
              }

-----------------------------------------------------------------------------

type EventProbabilityEstimation m = ProbabilityEstimation (EventCaches m) m

-----------------------------------------------------------------------------


