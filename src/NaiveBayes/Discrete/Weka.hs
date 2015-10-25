-----------------------------------------------------------------------------
--
-- Module      :  NaiveBayes.Discrete.Weka
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module NaiveBayes.Discrete.Weka (

  WekaEvent

, wekaEntry2Event
, wekaData2Events
, wekaSparse2Events

, buildCaches
, runWeka

) where

import Event (mkUnion)
import Event.Probability
import NaiveBayes.Discrete

import WekaData

import qualified Data.Set as Set
import qualified Data.Map as Map

--import Data.Set (Set)

import Data.List (intercalate)
import Data.IORef (readIORef)

import Control.Arrow ((&&&))

-----------------------------------------------------------------------------

-- | Analog of 'WekaEntry' for "NaiveBayes".
type WekaEvent = Event WekaVal

-- | Converts 'WekaEntry' to 'WekaEvent'.
wekaEntry2Event :: WekaEntry -> WekaEvent
wekaEntry2Event (WEntry set) = mkUnion $ Set.map Ev set

-- | Converts RawWekaData to a list of 'WekaEvent's.
wekaData2Events :: RawWekaData -> [WekaEvent]
wekaData2Events = uncurry wekaSparse2Events . (rwdAttrs &&& rawWekaData)

-- | Converts /sparse/ weka data to a list of 'WekaEvent's.
wekaSparse2Events :: [WekaDataAttribute] -> [[String]] -> [WekaEvent]
wekaSparse2Events attrs = map wekaEntry2Event . wekaSparse2WEntries attrs

-----------------------------------------------------------------------------

data Caches = Caches { countCache    :: CountCache WekaVal
                     , probCache     :: ProbMutMap WekaVal
                     , condProbCache :: CondProbMutMap WekaVal
                     }

buildCaches :: [WekaDataAttribute] -> [WekaEvent] -> IO Caches
buildCaches attrs evs = do
    let wvs = do a@(WekaAttrNom _ domain) <- attrs
                 return . Set.fromList $ map (curry WVal a) domain
    cCache <- emptyCountCache
    (pCache, cpCache) <- unknownProbMutMaps $ Set.unions wvs

    putStrLn "initial caches built"
    let caches  = Caches cCache pCache cpCache

    countEvents cCache evs
    putStrLn "updated count cache"

    updProb cCache pCache
    putStrLn "updated probability cache"

    updCondProb cCache cpCache
    putStrLn "updated conditional probability cache"

    return caches


-----------------------------------------------------------------------------

type Filename = String

swap (x,y) = (y,x)

runWeka :: Show WekaVal => Filename -> IO Caches
runWeka fname = do RawWekaData name attrs dta <- readWekaData fname
                   let entries = wekaSparse2Events attrs dta

                   putStr $ "NaiveBayes.Discrete for " ++ show name
                   putStrLn . intercalate "\n\t" $ map wekaAttribute2str attrs
                   putStrBar '-'
                   mapM_ print entries
                   putStrBar '='

                   cache <- buildCaches attrs entries

                   putStrBar '='
                   putStr $ replicate 25 '=' ++ " Finished "
                         ++ replicate 25 '=' ++ "\n"
                   putStrBar '='

                   putStrLn "Events Count:\n"
                   readIORef (countCache cache) >>= printMutMap "c"

                   putStrBar '-'
                   putStrLn "Probabilities:\n"
                   printMutMap "p" $ probCache cache

                   putStrBar '-'
                   putStrLn "Conditional Probabilities:\n"
                   printMutMap "p" $ condProbCache cache

                   return cache


printMutMap pref mmap = sequence_ $ do
    (k,ref) <- Map.assocs mmap
    return $ do p <- readIORef ref
                putStrLn $ concat [pref, "=", show p, ", ", show k]

putStrBar c = putStrLn $ "\n" ++ replicate 60 c ++ "\n"


