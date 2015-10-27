{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

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

, IORefMap
, IORefMap'(..)

, emptyCountCache
, emptyProbMutMaps

, countEvents
, maxLike

, tryEvalProb

) where

import Event
import Event.Extra
import Event.Probability
import Event.Probability.Cache
import Event.Probability.Eval

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import Data.IORef

import Control.Applicative ((<$>))

import GHC.Float (int2Float)

-----------------------------------------------------------------------------

-- | Alias for 'Event'.
type Condition = Event

type IORefMap k v = IORef (Map k (IORef v))

newtype IORefMap' k v = IORefMap' (IORefMap k v)

-----------------------------------------------------------------------------


-- | Alias for a 'Map', storing /mutable/ probability.
type KProbMutMap k = IORefMap k Probability

-- | Alias for a 'Map', storing /mutable/ probability of events.
type ProbMutMap ev = KProbMutMap (Event ev)

-- | Alias for a 'Map', storing /mutable/ conditional probability of events.
type CondProbMutMap ev = KProbMutMap (Event ev, Condition ev)

-----------------------------------------------------------------------------

-- | Alias for a 'Map', storing /mutable/ count.
type CountCache ev = IORefMap (Event ev) Int


-----------------------------------------------------------------------------

sumContains :: Ord ev => Map (Event ev) (IORef Int) -> Event ev -> IO Int

sumContains cache e@(Ev _) = sumContains cache . Intersect $ Set.singleton e

sumContains cache (Intersect set) =
    fmap sum . mapM readIORef . Map.elems
    $ Map.filterWithKey (\(Intersect k) _ -> set `Set.isSubsetOf` k) cache


updMutMapRef mref f key = do
    m <- readIORef mref

    case Map.lookup key m of
        Just ref -> modifyIORef ref (f . Just)
        _        -> do ref <- newIORef $ f Nothing
                       writeIORef mref $ Map.insert key ref m

-----------------------------------------------------------------------------

emptyProbMutMaps :: IO (ProbMutMap ev, CondProbMutMap ev)
emptyProbMutMaps = do
    pcache  <- newIORef Map.empty
    cpcache <- newIORef Map.empty
    return (pcache, cpcache)


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


-----------------------------------------------------------------------------


instance (Show key, Ord key) =>

    Cache IORefMap' key val where

        inCache (IORefMap' cref) k = do cache <- readIORef cref
                                        return $ k `Map.member` cache

        lookupProbCache (IORefMap' cref) k = do
            cache <- readIORef cref
            maybe (return Nothing)
                  (fmap Just . readIORef)
                  $ Map.lookup k cache

        updateProbCache (IORefMap' cref) k f = do
            cache <- readIORef cref
            case Map.lookup k cache of Just ref -> modifyIORef ref f
                                       _        -> return ()

instance (Show key, Ord key) =>

    MutableCache IORefMap' key vval where

        insertProbInCache (IORefMap' cref) k v = do
            vref <- newIORef v
            modifyIORef cref (Map.insert k vref)


-----------------------------------------------------------------------------

estimateAndUpdateP :: (Show ev, Ord ev) =>
                      IORefMap' (Event ev) Int
                   -> IORefMap' (Event ev) Probability
                   -> P ev
                   -> IO Probability

estimateAndUpdateP (IORefMap' cref) prefc (P ev _) = do
    cache <- readIORef cref
    cnt <- int2Float <$> cacheSum cref
    c   <- sumContains cache ev
    let p = probability $ int2Float c / cnt
    insertProbInCache prefc ev p
    return p

-----------------------------------------------------------------------------

estimateAndUpdatePC :: (Show ev, Ord ev) =>
                       IORefMap' (Event ev) Int
                    -> IORefMap' (Event ev, Event ev) Probability
                    -> PCond ev
                    -> IO Probability

estimateAndUpdatePC (IORefMap' cref) cpc (PCond ev cond _) = do
    p <- maxLike cref ev cond
    insertProbInCache cpc (ev, cond) p
    return p

-----------------------------------------------------------------------------

emptyP  ev      = P ev emptyProbability
emptyPC ev cond = PCond ev cond emptyProbability


estimatePCWithBayes :: (Show ev, Ord ev, EventAtoms Event ev, EventDomain ev) =>
                       IORefMap' (Event ev) Int
                    -> IORefMap' (Event ev) Probability
                    -> IORefMap' (Event ev, Event ev) Probability
                    -> PCond ev
                    -> IO Probability

-- assuming: independent events
estimatePCWithBayes cc pc cpc (PCond ev@(Ev ev') cond@(Intersect cset) _) = do
    -- Map @y@ -> @P(Y = y) ∏ P(Xi|Y = y)@
    cpyList <- sequence $ do
                y <- Set.toList $ eventDomain ev'
                let mpy  = estimateAndUpdateP cc pc (pUnknown ev)
                let f    = estimateAndUpdatePC cc cpc
                         . (\x -> PCond x (Ev y) emptyProbability)
                let pxms = map f $ Set.toList cset

                return $ do py  <- mpy
                            pxs <- sequence pxms
                            return (y, (py, pxs))
    putStrLn "cpyMap:"
    mapM_ (\(k, v) -> putStrLn $ "\t" ++ show k ++ "\t" ++ show v) cpyList

    let cpyMap = Map.map (\(y,xs) -> y * sum xs) $ Map.fromList cpyList

    let nominator   = cpyMap Map.! ev'
    let denominator = sum $ Map.elems cpyMap

    return $ nominator / denominator

-----------------------------------------------------------------------------

instance (Show ev, Ord ev, EventAtoms Event ev, EventDomain ev) =>

    ProbabilityEval IORefMap' IORefMap' IORefMap' ev where

    tryEvalProb pc cpc cc (EvProb prob) = do
        p <- case toEither prob of Left  p                       -> estimateAndUpdateP cc pc p
                                   Right cond@(PCond _ (Ev _) _) -> estimateAndUpdatePC cc cpc cond
                                   Right cond@(PCond (Ev _) _ _) -> estimatePCWithBayes cc pc cpc cond

        return . EvProb $ updProbability prob p





