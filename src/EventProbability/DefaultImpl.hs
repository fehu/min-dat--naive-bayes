{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      :  EventProbability.DefaultImpl
-- Description :  Instances for 'ProbabilityEstimation' and 'NaiveBayesCondProb'.
-- License     :  MIT
--
-- Instances for 'ProbabilityEstimation' and 'NaiveBayesCondProb'.
--

module EventProbability.DefaultImpl where

import Cache
import EventProbability
import EventProbability.Cache
import EventProbability.NaiveBayes

import GHC.Float

import Control.Monad       ( liftM )
import Control.Applicative ( (<$>) )

import qualified Data.Map as Map
import qualified Data.Set as Set

-----------------------------------------------------------------------------

-- | Estimates the probabilities and update the corresponding caches.
--   Implemented in "EventProbability.DefaultImpl".
instance ProbabilityEstimation (EventCaches IO) IO where

    estimateProb (EvCaches cc pc _) ev = fst <$> findOrElseInsertM pc d ev
        where d = do cEv    <- countOccurences cc ev
                     cTotal <- (sum . map snd) <$> listCache cc
                     return $ if cTotal == 0
                        then 0
                        else mkProb $ int2Double cEv / int2Double cTotal

    -- | Max likelihood
    estimateCondProb (EvCaches cc _ cpc) ev cond =
        fst <$> findOrElseInsertM cpc d (ev, cond)
        where d = do cEv   <- countOccurences cc $ ev & cond
                     cCond <- countOccurences cc $ mkEvent cond
                     return $ if cCond == 0
                        then 0
                        else mkProb $ int2Double cEv / int2Double cCond



-- | Implemented in "EventProbability.DefaultImpl".
instance NaiveBayesCondProb (EventCaches IO) IO where

    estimateDomainCondProbsWithBayes caches atom (Event condMap) =
        mapM (\d -> liftM ((,) d) (probOf d)) . Set.toList $ eventDomain atom

        where probOf d = do
                pd   <- estimateProb caches $ mkEvent d
                cpds <- sequence $ do
                        cond <- Map.elems condMap
                        return $ estimateCondProb caches (mkEvent cond) d
                return $ pd * product cpds


