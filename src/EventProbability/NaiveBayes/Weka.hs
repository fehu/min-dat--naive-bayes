{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  EventProbability.NaiveBayes.Weka
-- Description :  NaiveBayes for Weka data.
-- License     :  MIT
--
-- NaiveBayes learing ald classification for Weka data.
--


module EventProbability.NaiveBayes.Weka (

  wekaEntry2Event

, buildCaches
, classifyWekaData

, learnAndTest
, runWekaLearnAndTest

) where


import Cache
import EventProbability
import EventProbability.Cache
import EventProbability.NaiveBayes

import WekaData

import qualified Data.Set as Set

import Data.Maybe    ( fromMaybe )
import Data.List     ( find )
import Data.Typeable
import Control.Arrow ( (&&&) )
import Control.Monad ( when )

-----------------------------------------------------------------------------

instance AtomicEvent WekaVal where

    eventName (WVal (attr,_)) = EventName $ wekaAttributeName attr

    eventDomain (WVal (attr@(WekaAttrNom _ dom), _)) =
        Set.fromList $ map (WVal . (const attr &&& id)) dom

-----------------------------------------------------------------------------

wekaEntry2Event :: (Show WekaVal) => WekaEntry -> Event
wekaEntry2Event (WEntry vals) = compositeEvent vals

-----------------------------------------------------------------------------

buildCaches :: (Show WekaVal) => RawWekaData -> IO (EventCaches IO)

buildCaches rawData = do
    cc  <- newIOCache
    pc  <- newIOCache
    cpc <- newIOCache

    updateCountCache cc events
    return $ EvCaches cc pc cpc

    where events = map wekaEntry2Event $ wekaData2SparseWEntries rawData

-----------------------------------------------------------------------------

type ClassifyResult = (Event, Maybe (EvAtom, Prob))

classifyWekaData :: (Show WekaVal, NaiveBayesCondProb (EventCaches IO) IO) =>
                    EventCaches IO
                 -> RawWekaData
                 -> String       -- ^ attribute to clasify.
                 -> IO [ClassifyResult]

classifyWekaData caches rawData className = sequence $ do
    ev <- events
    return $ do c <- classifyEvent caches cAtom ev
                return (ev, c)
--    return $ classifyEvent caches cAtom ev >>= (fmap ((,) ev))

--    undefined
    where classifyBy = fromMaybe (error $ errStr ++ show className)
                     $ find ((==) className . wekaAttributeName)
                        $ rwdAttrs rawData
          errStr = "couldn't find an attribute with name "
          cAtom  = EvAtom $ WVal (classifyBy, undefined)

          events = map wekaEntry2Event $ wekaData2SparseWEntries rawData


-----------------------------------------------------------------------------

learnAndTest :: ( Show WekaVal, Show WekaEntry
                , NaiveBayesCondProb (EventCaches IO) IO) =>
                RawWekaData -- ^ /learn/ data.
             -> RawWekaData -- ^ /test/ data.
             -> String      -- ^ attribute to classify.
             -> Bool        -- ^ show test results.
             -> IO (EventCaches IO, [ClassifyResult])

learnAndTest learnData testData className showRes = do
    caches       <- buildCaches learnData
    testClassify <- classifyWekaData caches testData className

    -- for showRes
    let err =  "class '" ++ className ++ "' not found in "
    let getClass e@(WEntry set) = fromMaybe (error $ err ++ show e)
                                $ find (\(WVal (a,_))-> className == wekaAttributeName a)
                                $ Set.toList set

    let expectedRes = map getClass $ wekaData2SparseWEntries testData

    let notClassified ev exp = do
            putStrLn $ "\n[?]" ++ show ev ++ " wasn't classified, expected " ++ show exp
            return False
    let compareClassified ev exp (EvAtom c, p) =
            case cast exp of Just exp' | exp' == c -> do putStrLn strOk
                                                         return True
                             _                     -> do putStrLn strFail
                                                         return False
            where strOk = "\n[+]\t" ++ show ev ++ " was classified correcly ("
                       ++ show c ++ ") with probability " ++ show p
                  strFail = "\n[-]\t" ++ show ev ++ " was classified incorrecly: expected "
                         ++ show exp ++ ", got " ++ show c
                         ++ "(with probability " ++ show p ++ ")"

    when showRes $ do
        results <- sequence $ do
            ((ev, mbClass), exp) <- zip testClassify expectedRes
            return $ maybe (notClassified ev exp) (compareClassified ev exp) mbClass

        let ok = length $ filter id results

        putStrLn $ replicate 30 '='
        putStrLn $ show ok ++ "\t classified correctly"
        putStrLn $ show (length testClassify - ok) ++ "\t classified incorrectly"

    return (caches, testClassify)


-----------------------------------------------------------------------------

type Filename = String

runWekaLearnAndTest :: ( Show WekaVal, Show WekaEntry
                       , NaiveBayesCondProb (EventCaches IO) IO) =>
                       Filename -- ^ path to a weka data file with /learn/ data.
                    -> Filename -- ^ path to a weka data file with /test/ data.
                    -> String   -- ^ attribute to classify.
                    -> Bool     -- ^ show test results.
                    -> IO (EventCaches IO, [ClassifyResult])

runWekaLearnAndTest learnFile testFile className showRes = do
    learnData <- readWekaData learnFile
    testData  <- readWekaData testFile

    learnAndTest learnData testData className showRes




-----------------------------------------------------------------------------

