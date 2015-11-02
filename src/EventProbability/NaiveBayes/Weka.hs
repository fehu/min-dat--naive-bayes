{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- Module      :  EventProbability.NaiveBayes.Weka
-- Description :  NaiveBayes for Weka data.
-- License     :  MIT
--
-- NaiveBayes learing and classification for Weka data.
--


module EventProbability.NaiveBayes.Weka (

-- * Prepare
  wekaEntry2Event

-- * Execute
, buildCaches
, classifyWekaData
, ClassifyResult

-- * Test
, learnAndTest
, Filename
, runWekaLearnAndTest

) where


import Cache
import EventProbability
import EventProbability.Cache
import EventProbability.NaiveBayes

import WekaData

import qualified Data.Set as Set

import Data.Maybe    ( fromMaybe, maybeToList )
import Data.List     ( find )
import Data.Typeable
import Control.Arrow ( (&&&) )
import Control.Monad ( when )

-----------------------------------------------------------------------------

instance AtomicEvent WekaVal where

    eventName (WVal attr _) = EventName $ wekaAttributeName attr

    eventDomain (WVal attr@(WekaAttrNom _ dom) _) =
        Set.fromList $ map (uncurry WVal . (const attr &&& id)) dom

-----------------------------------------------------------------------------

-- | Build an 'Event' from a 'WekaEntry'.
wekaEntry2Event :: (Show WekaVal) => WekaEntry -> Event
wekaEntry2Event (WEntry vals) = compositeEvent vals

-----------------------------------------------------------------------------

-- | Create initial 'EventCaches' and update counts of the 'EventCountCache'.
buildCaches :: (Show WekaVal) => RawWekaData -> IO (EventCaches IO)

buildCaches rawData = do
    cc  <- newIOCache
    pc  <- newIOCache
    cpc <- newIOCache

    updateCountCache cc events
    return $ EvCaches cc pc cpc

    where events = map wekaEntry2Event $ wekaData2Sparse rawData

-----------------------------------------------------------------------------

-- | Classification result of an 'Event'.
type ClassifyResult = (Event, Maybe (EvAtom, Prob))


-- | Classifies given 'RawWekaData' by given attribute name using 'EventCaches'.
classifyWekaData :: (Show WekaVal, NaiveBayesCondProb (EventCaches IO) IO) =>
                    EventCaches IO
                 -> RawWekaData    -- ^ data to classify.
                 -> String         -- ^ attribute to clasify.
                 -> IO [ClassifyResult]

classifyWekaData caches rawData className = sequence $ do
    ev <- events
    return $ do c <- classifyEvent caches cAtom ev
                return (ev, c)

    where classifyBy = fromMaybe (error $ errStr ++ show className)
                     $ find ((==) className . wekaAttributeName)
                        $ rwdAttrs rawData
          errStr = "couldn't find an attribute with name "
          cAtom  = EvAtom $ WVal classifyBy undefined

          events = map wekaEntry2Event $ wekaData2Sparse rawData


-----------------------------------------------------------------------------

-- | Teaches a /Naive Bayes/ classifier (updates the caches) with the
--   /learn/ data and tests the classification quality with the /test/ data.
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
    let err = "class '" ++ className ++ "' not found in "
    let getClass e@(WEntry set) = find (\(WVal a _)-> className == wekaAttributeName a)
                                $ Set.toList set

    let expectedRes = map getClass $ wekaData2Sparse testData

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
            ((ev, mbClass), mbExp) <- zip testClassify expectedRes
            let compare exp = maybe (notClassified ev exp) (compareClassified ev exp) mbClass
            maybe [] (return . compare) mbExp

        let ok = length $ filter id results

        putStrLn $ replicate 30 '='
        putStrLn $ show ok ++ "\t classified correctly"
        putStrLn $ show (length results - ok) ++ "\t classified incorrectly"

    return (caches, testClassify)


-----------------------------------------------------------------------------

type Filename = String

-- | Calls 'learnAndTest' after extracting 'RawWekaData' from the given files.
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

