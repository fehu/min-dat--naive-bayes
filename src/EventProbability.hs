{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

-- |
--
-- Module      :  EventProbability
-- Description :  Events and probabilities.
-- License     :  MIT
--
-- Definitions for events and probabilities.
--

module EventProbability (

-- * Events
  Event(..)
, EventName(..)
, mkEvent
, emptyEvent
, compositeEvent

-- * Atomic Events
, AtomicEvent(..)
, EvAtom(..)
, EventIntersection(..)

-- * Probability

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

-- | A name of an event.
newtype EventName = EventName String             deriving (Eq, Ord)

-- | An event: intersection of underlying 'EvAtom's.
newtype Event     = Event (Map EventName EvAtom) deriving (Eq, Ord)


evPair = eventName &&& id

-- | Creates a singleton 'Event'.
mkEvent :: EvAtom -> Event
mkEvent = Event . uncurry Map.singleton . evPair

-- | Creates an empty 'Event'.
emptyEvent :: Event
emptyEvent = Event Map.empty

-- | Creates an /intersection/ 'Event'.
compositeEvent :: (AtomicEvent e, Typeable e, Show e, Ord e) => Set e -> Event
compositeEvent set | Set.null set = emptyEvent
                   | otherwise    = Set.foldr ((&) . EvAtom) emptyEvent set


instance Show EventName where show (EventName name) = name
instance Show Event     where show (Event set)      = intercalate " & "
                                                    . map (show . snd)
                                                    $ Map.toAscList set

-----------------------------------------------------------------------------

-- | An existential container for 'AtomicEvent'.
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

-- | Atomic (not composite) event.
class AtomicEvent e where
    -- | Event's name.
    eventName   :: e -> EventName
    -- | Event's values domain.
    eventDomain :: e -> Set e

-----------------------------------------------------------------------------

-- | Events intersection builder.
class EventIntersection e1 e2 where
    -- | Events intersection.
    intersect :: e1 -> e2 -> Event
    -- | Alias for 'intersect'.
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

-- | Create a 'Prob'.
mkProb  :: Double -> Prob
-- | Get 'Double' value from 'Prob'.
getProb :: Prob -> Double

mkProb d | d >=0 && d <= 1 = Prob d
         | otherwise       = error $ probErrStr ++ show d

getProb (Prob d) = d

probErrStr = "probability must be within [0,1], but got "

probf  f (Prob mx)           = mkProb $ f mx
probf2 f (Prob mx) (Prob my) = mkProb $ f mx my

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

-- | Interface for 'Event's probability estimation.
class (Monad m) =>
    ProbabilityEstimation context m where
        -- | Estimate the probability of an event, given a context.
        estimateProb     :: context -> Event -> m Prob
        -- | Estimate the the conditional probability of an event, given a context.
        estimateCondProb :: context
                         -> Event  -- ^ the event
                         -> EvAtom -- ^ condition
                         -> m Prob

