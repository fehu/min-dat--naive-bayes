{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

-- |
--
-- Module      :  Event.Probability
-- Description :
-- License     :  MIT
-- Stability   :  dev
--
--

module Event.Probability (

  Probability(..)
, probability
, emptyProbability

, EventProbability(..)
, EvProb(..)

, ProbabilityConstructor ((~~))
, (~|)

, P(..)
, pUnknown

, PCond(..)

----------------------------

, Event(Ev, Universal, Null)

, union
, (&)

, intersection
, (#)

, complement
, neg

) where

import Event

--import Event.Extra

import Control.Monad
import Data.Maybe (isJust, fromJust)
import Data.Ratio
import GHC.Float

-----------------------------------------------------------------------------

-- | Probability container
newtype Probability = Probability (Maybe Float)
                    deriving (Eq, Ord)

instance Show Probability where
    show (Probability (Just p)) = show p
    show _                      = "?"


probf  f (Probability mx)                  = Probability $ fmap f mx
probf2 f (Probability mx) (Probability my) = Probability $ liftM2 f mx my

instance Num Probability where (+) = probf2 (+)
                               (-) = probf2 (-)
                               (*) = probf2 (*)
                               abs = probf abs
                               signum = probf signum
                               fromInteger 0 = probability 0
                               fromInteger 1 = probability 1
                               fromInteger _ = Probability Nothing

instance Fractional Probability where (/) = probf2 (/)
                                      fromRational r = probability ( fromInteger (numerator r)
                                                                   / fromInteger (denominator r))

probability :: Float -> Probability
probability p | p >= 0 && p <= 1 = Probability $ Just p
              | otherwise        = error $
                                    "probability must be in range [0,1], got "
                                    ++ show p

emptyProbability :: Probability
emptyProbability = Probability Nothing

-----------------------------------------------------------------------------

data EvProb ev = forall p . (EventProbability p ev, Show (p ev)) => EvProb (p ev)

instance Show (EvProb ev) where show (EvProb p) = show p

-- | Interface for 'Probability' of an 'Event'. Can be 'P' or 'PCond'.
class EventProbability p ev where
                              -- | get 'Probability' value.
                                  getProbability :: p ev -> Probability
                              -- | copy this 'Probability' with new value.
                                  updProbability :: p ev -> Probability -> p ev

                              -- | try to represent as 'P' (pure probability).
                                  asProb         :: p ev -> Maybe (P ev)
                              -- | try to represent as 'PCond'.
                                  asCondProb     :: p ev -> Maybe (PCond ev)
                              -- | represent as an 'Either' (conditional probability).
                                  toEither       :: p ev -> Either (P ev) (PCond ev)

                              -- | is conditional?
                                  isCondProb  :: p ev -> Bool
                              -- | isn't conditional?
                                  notCondProb :: p ev -> Bool

                                  isCondProb  = isJust . asCondProb
                                  notCondProb = not . isCondProb
                                  toEither p  = maybe cond Left $ asProb p
                                            where cond = Right . fromJust $ asCondProb p


-- | Probability of an event
data P ev = P  (Event ev) Probability                   deriving (Eq, Ord)

-- | Conditional probability of an event
data PCond ev = PCond (Event ev) (Event ev) Probability deriving (Eq, Ord)




instance EventProbability P     ev where getProbability (P _ p) = p
                                         updProbability (P e _) = P e
                                         asProb                 = Just
                                         asCondProb _           = Nothing

instance EventProbability PCond ev where getProbability (PCond _ _ p) = p
                                         updProbability (PCond e c _) = PCond e c
                                         asProb _                     = Nothing
                                         asCondProb                   = Just

instance Show ev =>
    Show (P ev)  where
        show (P ev p)       = "P(" ++ show ev ++ ")=" ++ show p

instance Show ev =>
    Show (PCond ev) where
        show (PCond ev c p) = "P(" ++ show ev ++ "|" ++ show c  ++ ")=" ++ show p


pUnknown :: Event ev -> P ev
pUnknown = flip P (Probability Nothing)

-----------------------------------------------------------------------------

type family UProb a where
    UProb (Event ev)  = P     ev
    UProb (PCond' ev) = PCond ev


class UProb (from ev) ~ (p ev) =>
    ProbabilityConstructor p ev from pv     where (~~) :: from ev -> pv -> p ev


instance Ord ev =>
    ProbabilityConstructor P     ev Event  Float where ev           ~~ p = P ev $
                                                                      probability p
instance Ord ev =>
    ProbabilityConstructor PCond ev PCond' Float where (PCond' e c) ~~ p = PCond e c $
                                                                      probability p


instance Ord ev =>
    ProbabilityConstructor P     ev Event  Probability where ev           ~~ p = P ev p
instance Ord ev =>
    ProbabilityConstructor PCond ev PCond' Probability where (PCond' e c) ~~ p = PCond e c p


data PCond' ev = PCond' (Event ev) (Event ev)

-- | conditional probability constructor
(~|) :: Event ev -> Event ev -> PCond' ev

e ~| cond = PCond' e cond


