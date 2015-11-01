{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  EventProbability
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module EventProbability (

  Event(..)
, EventName(..)
, mkEvent
, emptyEvent
, compositeEvent

, AtomicEvent(..)
, EvAtom(..)
, EventIntersection(..)

, Prob, mkProb, getProb

, ProbabilityEstimation(..)

) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Set (Set)
import Data.Map (Map)
import Data.List (intercalate)
import Data.Typeable

import Control.Arrow ( (&&&) )


-----------------------------------------------------------------------------

newtype EventName = EventName String             deriving (Eq, Ord)
newtype Event     = Event (Map EventName EvAtom) deriving (Eq, Ord)


evPair = eventName &&& id

mkEvent :: EvAtom -> Event
mkEvent = Event . uncurry Map.singleton . evPair

emptyEvent :: Event
emptyEvent = Event Map.empty

compositeEvent :: (AtomicEvent e, Typeable e, Show e, Ord e) => Set e -> Event
compositeEvent set | Set.null set = emptyEvent
                   | otherwise    = Set.foldr ((&) . EvAtom) emptyEvent set

instance Show EventName where show (EventName name) = name
instance Show Event     where show (Event set)      = intercalate " & "
                                                    . map (show . snd)
                                                    $ Map.toAscList set


-----------------------------------------------------------------------------


data EvAtom = forall e . (AtomicEvent e, Show e, Ord e, Typeable e) =>  EvAtom e

instance Show EvAtom where show (EvAtom a) = show a

instance Eq   EvAtom where
    (EvAtom a) == (EvAtom b) =
        case cast b of Just b -> a == b
                       _      -> False

instance Ord  EvAtom where
    (EvAtom a) `compare` (EvAtom b) =
        case cast b of Just b' -> a `compare` b'
                       _       -> eventName a `compare` eventName b

instance AtomicEvent EvAtom where
    eventName   (EvAtom e) = eventName e
    eventDomain (EvAtom e) = Set.map EvAtom $ eventDomain e


-----------------------------------------------------------------------------

class AtomicEvent e where
    eventName   :: e -> EventName
    eventDomain :: e -> Set e

-----------------------------------------------------------------------------

class EventIntersection e1 e2 where
    intersect :: e1 -> e2 -> Event
    (&)       :: e1 -> e2 -> Event

    (&) = intersect

instance EventIntersection EvAtom EvAtom where
    intersect a b = Event . Map.fromList $ map evPair [a,b]

instance EventIntersection EvAtom Event where
    intersect e (Event es) = Event $ Map.insert (eventName e) e es

instance EventIntersection Event EvAtom where
    intersect = flip intersect

instance EventIntersection Event Event where
    intersect (Event es1) (Event es2) = Event $ Map.union es1 es2

-----------------------------------------------------------------------------

-- | Probability value container.
newtype Prob = Prob Double deriving (Eq, Ord)

instance Show Prob where show (Prob p) = show p

mkProb  :: Double -> Prob
getProb :: Prob -> Double

mkProb d | d >=0 && d <= 1 = Prob d
         | otherwise       = error $ probErrStr ++ show d

getProb (Prob d) = d

probErrStr = "probability must be within [0,1], but got "

probf  f (Prob mx)           = Prob $ f mx
probf2 f (Prob mx) (Prob my) = Prob $ f mx my

instance Num Prob where (+) = probf2 (+)
                        (-) = probf2 (-)
                        (*) = probf2 (*)
                        abs = probf abs
                        signum = probf signum
                        fromInteger 0 = Prob 0
                        fromInteger 1 = Prob 1
                        fromInteger p = error $ npErrStr ++ show p

npErrStr = "probability must be in [0,1], got "

-----------------------------------------------------------------------------

class (Monad m) =>
    ProbabilityEstimation context m where
        -- | Estimate the probability of an event, given a context.
        estimateProb     :: context -> Event -> m Prob
        -- | Estimate the the conditional probability of an event, given a context.
        estimateCondProb :: context
                         -> Event  -- ^ the event
                         -> EvAtom -- ^ condition
                         -> m Prob

