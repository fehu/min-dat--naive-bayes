{-# LANGUAGE ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  Event.Extra
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

module Event.Extra (

  EventDomain(..)
, EventAtoms(..)
, AtomicEventDomain

) where

import Event

import Data.Set (Set)
import qualified Data.Set as Set


-- | For @(P ev _)@: searches the event in cache.
--
--   For @(PCond ev cond _)@: estimates the probability of the
--   conditional event, using cache.

class EventDomain      ev where eventDomain :: ev -> Set ev
class EventAtoms event ev where getAtoms    :: event ev -> Maybe (Set ev)

type AtomicEventDomain event ev = (EventDomain ev, EventAtoms event ev)


instance (Ord ev) =>
    EventAtoms Event ev where
        getAtoms (Ev ev)         = Just $ Set.singleton ev
        getAtoms (Union set)     = Just $ Set.map (\(Ev ev) -> ev) set
        getAtoms (Intersect set) = Just $ Set.map (\(Ev ev) -> ev) set
        getAtoms _               = Nothing


