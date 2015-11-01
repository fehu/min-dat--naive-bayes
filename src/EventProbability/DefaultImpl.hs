-- |
--
-- Module      :  EventProbability.DefaultImpl
-- Description :  Default implementations.
-- License     :  MIT
--
-- Default implementations for 'ProbabilityEstimation' and 'NaiveBayesCondProb'.
--

module EventProbability.DefaultImpl (

) where


import Cache
import EventProbability
import EventProbability.Cache
import EventProbability.NaiveBayes

import GHC.Float

import Control.Monad       ( liftM )
import Control.Applicative ( (<$>) )

-----------------------------------------------------------------------------

-- | Estimates the probabilities and update the corresponding caches.
instance ProbabilityEstimation (EventCaches IO) IO where

    estimateProb (EvCaches cc pc _) ev = fst <$> findOrElseInsertM pc d ev
        where d = do cEv    <- countOccurences cc ev
                     cTotal <- (sum . map snd) <$> listCache cc
                     return . mkProb $ int2Double cEv / int2Double cTotal

    -- | Max likelihood
    estimateCondProb (EvCaches cc _ cpc) ev cond =
        fst <$> findOrElseInsertM cpc d (ev, cond)
        where d = do cEv   <- countOccurences cc $ ev & cond
                     cCond <- countOccurences cc $ mkEvent cond
                     return . mkProb $ int2Double cEv / int2Double cCond



instance NaiveBayesCondProb (EventCaches IO) IO where

--    estimateDomainCondProbsWithBayes
