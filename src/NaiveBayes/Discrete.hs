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

  KProbMutMap, ProbMutMap, CondProbMutMap
, CountCache

, unknownProbMutMaps
--, getProbOf
--, changeProbOf

, emptyCountCache
, countEvents

, maxLike

, updProb
, updCondProb

) where

import Event
import Event.Probability

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Data.IORef

import Control.Arrow ((&&&))
import Control.Monad (mzero, liftM)
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
type CountCache ev = IORef (Map (Event ev) (IORef Int))


-----------------------------------------------------------------------------

sumContains :: Ord ev => Map (Event ev) (IORef Int) -> Event ev -> IO Int

sumContains cache e@(Ev _) = sumContains cache . Intersect $ Set.singleton e

sumContains cache (Intersect set) =
    fmap sum . mapM readIORef . Map.elems
    $ Map.filterWithKey (\(Intersect k) _ -> set `Set.isSubsetOf` k) cache

--changeMutOf pmap f key = do p <- readIORef pref
--                            writeIORef pref (f p)
--    where pref = pmap ! key


--countMutContains contains pmap key = readIORef undefined
--    where toCount = filterWithKey (\k v ->) pmap

updMutMapRef mref f key = do
    m <- readIORef mref

    case Map.lookup key m of
        Just ref -> modifyIORef ref (f . Just)
        _        -> do ref <- newIORef $ f Nothing
                       writeIORef mref $ Map.insert key ref m

-----------------------------------------------------------------------------


-- | Create 'ProbMutMap' and 'CondProbMutMap' with unknown probability.
unknownProbMutMaps :: Ord ev => Set ev -> IO (ProbMutMap ev, CondProbMutMap ev)

---- | Get /probability/, stored in a KProbMutMap.
--getProbOf :: Ord k =>
--             k              -- ^ key to search
--          -> KProbMutMap k  -- ^ 'Map' to search
--          -> IO Probability -- ^ the conditional probability

---- | Change probability of a conditional event stored in 'CondProbMutMap'.
--changeProbOf :: Ord k =>
--                (Probability -> Probability) -- ^ change probability
--             -> k                            -- ^ key to affect
--             -> KProbMutMap k                -- ^ 'Map' to change
--             -> IO ()                        -- ^ change 'IO'

--getProbOf    = flip getMut
--changeProbOf f k m = changeMutOf m f k

unknownProbMutMaps es = do
    plist  <- sequence $ do e <- Set.elems es
                            return $ do ref <- newIORef (Probability Nothing)
                                        return (Ev e, ref)
    cplist <- sequence $ do x <- Set.elems es
                            y <- Set.elems es
                            if x /= y
                             then return $ do ref <- newIORef (Probability Nothing)
                                              return ((Ev x, Ev y), ref)
                             else mzero
    return (Map.fromList plist, Map.fromList cplist)

-----------------------------------------------------------------------------


-- | Create 'CountCache' with zero count.
emptyCountCache :: IO (CountCache ev)
emptyCountCache = newIORef Map.empty


countEvents :: Ord ev => CountCache ev -> [Event ev] -> IO ()
countEvents cref evs = do
    cache <- readIORef cref
    sequence_ $ fmap (updMutMapRef cref (maybe 1 (1+))) evs

cacheSum :: CountCache ev -> IO Int
cacheSum cref = do
    cache <- readIORef cref
    cs <- sequence $ do ref  <- Map.elems cache
                        return $ readIORef ref
    return $ sum cs

-----------------------------------------------------------------------------


-- | Maximum likelihood estimate, using 'CountCache'.
maxLike :: Ord ev => CountCache ev -> Event ev -> Condition ev -> IO Probability
maxLike cref ev cond = do
    let Intersect es = ev
    cache <- readIORef cref
    cEvUn <- sumContains cache (ev # cond)
    cCond <- sumContains cache cond
    return . probability $ int2Float cEvUn / int2Float cCond


updProb :: Ord ev => CountCache ev -> ProbMutMap ev -> IO ()
updProb cref pmap = do
    cache <- readIORef cref
    cnt <- int2Float <$> cacheSum cref
    let f (k, ref) = do c <- sumContains cache k
                        let p = int2Float c / cnt
                        writeIORef ref . probability $ p
    sequence_ . fmap f . Map.assocs $ pmap


updCondProb :: Ord ev => CountCache ev -> CondProbMutMap ev -> IO ()
updCondProb cache cpmap = sequence_ . fmap f . Map.assocs $ cpmap
    where f ((e, cond), ref) = do p <- maxLike cache e cond
                                  writeIORef ref p


-----------------------------------------------------------------------------



