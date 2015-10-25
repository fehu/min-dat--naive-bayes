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

import Event
import Event.Probability

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Data.IORef

import Control.Arrow ((&&&))
import Control.Monad (mzero)
import Control.Applicative ((<$>))

import GHC.Float (int2Float)

-----------------------------------------------------------------------------

-- | Alias for 'Event'.
type Condition = Event

-----------------------------------------------------------------------------


-- | Alias for a 'Map', storing /mutable/ probability.
type KProbMutMap k = Map k (IORef Probability)

-- | Alias for a 'Map', storing /mutable/ probability of events.
type ProbMutMap ev = KProbMutMap (Event ev)

-- | Alias for a 'Map', storing /mutable/ conditional probability of events.
type CondProbMutMap ev = KProbMutMap (Event ev, Condition ev)

-----------------------------------------------------------------------------


-- | Alias for a 'Map', storing /mutable/ count.
type CountCache ev =  Map (Event ev) (IORef Int)


-----------------------------------------------------------------------------


getMut pmap key = readIORef $ pmap ! key

changeMutOf pmap f key = do p <- readIORef pref
                            writeIORef pref (f p)
    where pref = pmap ! key

-----------------------------------------------------------------------------


-- | Create 'ProbMutMap' and 'CondProbMutMap' with unknown probability.
unknownProbMutMaps :: Ord ev => Event ev -> IO (ProbMutMap ev, CondProbMutMap ev)

-- | Get /probability/, stored in a KProbMutMap.
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

getProbOf    = flip getMut
changeProbOf f k m = changeMutOf m f k

unknownProbMutMaps ev = do
    let Union es = ev
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


-- | Create 'CountCache' with zero count.
zeroCountCache :: Ord ev => Set (Event ev) -> IO (CountCache ev)
zeroCountCache es = do
    let sets = Set.elems $ powerset es Set.\\ Set.empty
    clst <- sequence $ do set <- sets
                          return $ do ref <- newIORef 0
                                      return (mkUnion set, ref)
    return $ Map.fromList clst

countEvents :: Ord ev => CountCache ev -> [Event ev] -> IO ()
countEvents cache = sequence_ . fmap (changeMutOf cache (1+))

cacheSum :: CountCache ev -> IO Int
cacheSum cache = do
    cs <- sequence $ do ref <- Map.elems cache
                        return $ readIORef ref
    return $ sum cs

-----------------------------------------------------------------------------


-- | Maximum likelihood estimate, using 'CountCache'.
maxLike :: Ord ev => CountCache ev -> Event ev -> Condition ev -> IO Probability
maxLike cache ev cond = do
    let Union es = ev
    cEvUn <- getMut cache ev
    cCond <- getMut cache cond
    return . probability $ int2Float cEvUn / int2Float cCond


updProb :: Ord ev => CountCache ev -> ProbMutMap ev -> IO ()
updProb cache pmap = do
    cnt <- int2Float <$> cacheSum cache
    let f (k, ref) = do c <- getMut cache k
                        let p = int2Float c / cnt
                        writeIORef ref . probability $ p
    sequence_ . fmap f . Map.assocs $ pmap


updCondProb :: Ord ev => CountCache ev -> CondProbMutMap ev -> IO ()
updCondProb cache cpmap = sequence_ . fmap f . Map.assocs $ cpmap
    where f ((e, cond), ref) = do p <- maxLike cache e cond
                                  writeIORef ref p



