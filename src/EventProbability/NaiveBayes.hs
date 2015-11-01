{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      :  EventProbability.NaiveBayes
-- Description :  An interface for /Naive Bayes/.
-- License     :  MIT
--
-- An interface for /Naive Bayes/.
--


module EventProbability.NaiveBayes ( NaiveBayesCondProb(..) ) where

import EventProbability

import Data.List ( maximumBy )
import Data.Function ( on )

-----------------------------------------------------------------------------

-- | An interface for /Naive Bayes/.
class (ProbabilityEstimation context m) =>

    NaiveBayesCondProb context m where

    -- | Estimate the conditional probabilities of the domain of an event, given a context.
    estimateDomainCondProbsWithBayes :: context
                                     -> EvAtom -- ^ event that would have it's domain processed.
                                     -> Event  -- ^ condition
                                     -> m [(EvAtom, Prob)] -- ^ Map: atom from domain -> probability.

    -- | tries to classify an 'Event' by given 'EvAtom'.
    classifyEvent :: context
                  -> EvAtom  -- ^ classify by.
                  -> Event   -- ^ event to classify.
                  -> m (Maybe (EvAtom, Prob))

    classifyEvent ctx ev event = do
        probs <- estimateDomainCondProbsWithBayes ctx ev event
        let max@(_, maxp) = maximumBy (compare `on` snd) probs
        let maxs          = filter ((==) maxp . snd) probs

        if null probs
        then return Nothing
        else if length maxs == 1 then return $ Just max
                                 else return Nothing

-----------------------------------------------------------------------------

