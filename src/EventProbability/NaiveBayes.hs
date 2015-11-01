-- |
--
-- Module      :  EventProbability.NaiveBayes
-- Description :
-- License     :  MIT
--
--
--


module EventProbability.NaiveBayes ( NaiveBayesCondProb(..) ) where

import EventProbability
import Data.Map (Map)

-----------------------------------------------------------------------------

class (ProbabilityEstimation context m) =>

    NaiveBayesCondProb context m where

    -- | Estimate the conditional probabilities of the domain of an event, given a context.
    estimateDomainCondProbsWithBayes :: context
                                     -> EvAtom -- ^ event that would have it's domain processed.
                                     -> Event  -- ^ condition
                                     -> m (Map EvAtom Prob) -- ^ Map: atom from domain -> probability.

    -- | tries to classify an 'Event' by given EventName.
    classifyEvent :: context
                  -> EventName  -- ^ classify by: 'AtomicEvent' name.
                  -> Event      -- ^ event to classify.
                  -> m (Maybe (EvAtom, Prob))

--    classifyEvent ctx ename (Event eset) = undefined
--        where mbc = do ev <-
--        in do probByEv <- estimateDomainCondProbsWithBayes ctx

-----------------------------------------------------------------------------

