-----------------------------------------------------------------------------
--
-- Module      :  NaiveBayes.Discrete
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

module NaiveBayes.Discrete (

) where

import Event.Probability ( Probability(..), probability )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Data.IORef

import Control.Arrow ((&&&))
import Control.Monad (mzero)

import GHC.Float (int2Float)

-----------------------------------------------------------------------------

data Event ev = Event String ev deriving (Show, Eq, Ord)


-- | Alias for 'Event'.
type Condition = Event

type Events ev = Set (Event ev)

-----------------------------------------------------------------------------


-- | Alias for a 'Map', storing /mutable/ probability.
type KProbMutMap k = Map k (IORef Probability)

-- | Alias for a 'Map', storing /mutable/ probability of events.
type ProbMutMap ev = KProbMutMap (Event ev)

-- | Alias for a 'Map', storing /mutable/ conditional probability of events.
type CondProbMutMap ev = KProbMutMap (Event ev, Condition ev)

-----------------------------------------------------------------------------


-- | Alias for a 'Map', storing /mutable/ count.
type CountCache a =  Map (Set a) (IORef Int)


-----------------------------------------------------------------------------


getMutFrom key pmap = readIORef $ pmap ! key

changeMutOf f key pmap = do p <- readIORef pref
                            writeIORef pref (f p)
    where pref = pmap ! key

-----------------------------------------------------------------------------


-- | Create 'ProbMutMap' and 'CondProbMutMap' with unknown probability.
unknownProbMutMaps :: Ord ev => Set (Event ev) -> IO (ProbMutMap ev, CondProbMutMap ev)

-- | Lookup /probability/, stored in a KProbMutMap.
getProbOf :: Ord k =>
             k              -- ^ key to search
          -> KProbMutMap k  -- ^ 'Map' to search
          -> IO Probability -- ^ the conditional probability

-- | Change probability of a conditional event stored in 'CondProbMutMap'.
changeProbOf :: Ord k =>
                (Probability -> Probability) -- ^ change probability
             -> k                            -- ^ key to affect
             -> KProbMutMap k                -- ^ 'Map' to change
             -> IO ()                        -- ^ change 'IO'

getProbOf    = getMutFrom
changeProbOf = changeMutOf

unknownProbMutMaps es = do
    plist  <- sequence $ do e <- Set.elems es
                            return $ do ref <- newIORef (Probability Nothing)
                                        return (e, ref)
    cplist <- sequence $ do x <- Set.elems es
                            y <- Set.elems es
                            if x /= y
                             then return $ do ref <- newIORef (Probability Nothing)
                                              return ((x, y), ref)
                             else mzero
    return (Map.fromList plist, Map.fromList cplist)

-----------------------------------------------------------------------------

powerset :: Ord a => Set a -> Set (Set a)
powerset set | Set.null set = Set.empty
             | otherwise    = Set.unions (Set.singleton set : psets)
    where psets = [ powerset $ Set.deleteAt i set | i <- [0..Set.size set] ]

-- |
emptyCountCache :: Ord ev => Events ev -> IO (CountCache (Event ev))
emptyCountCache es = do
    let sets = Set.elems $ powerset es Set.\\ Set.empty
    clst <- sequence $ do set <- sets
                          return $ do ref <- newIORef 0
                                      return (set, ref)
    return $ Map.fromList clst


-----------------------------------------------------------------------------


-- | Maximum likelihood estimate, using 'CountCache'.
maxLike :: (Ord ev) => CountCache (Event ev) -> Events ev -> Condition ev -> IO Probability
maxLike cache es cond = do
    cEvUn <- getMutFrom es cache
    cCond <- getMutFrom (Set.singleton cond) cache
    return . probability $ int2Float cEvUn / int2Float cCond

