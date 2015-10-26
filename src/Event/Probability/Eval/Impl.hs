{-# LANGUAGE ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  Event.Probability.Eval.Impl
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

module Event.Probability.Eval.Impl (

) where

import Event
import Event.Probability
import Event.Probability.Eval

import Data.Set (Set)
import qualified Data.Set as Set


-- | For @(P ev _)@: searches the event in cache.
--
--   For @(PCond ev cond _)@: estimates the probability of the
--   conditional event, using cache.

class EventDomain      ev where eventDomain :: ev -> Set ev
class EventAtoms event ev where getAtoms    :: event ev -> Maybe (Set ev)

type AtomicEventDomain event ev = (EventDomain ev, EventAtoms event ev)


---- | converts 'Ev', 'Union' y 'Intersect' to @Set ev@
--getAtoms' ev@(Ev _)       = Just $ Set.singleton ev
--getAtoms' (Union set)     = Just set
--getAtoms' (Intersect set) = Just set
--getAtoms' _               = Nothing



--extractAtomEv (Ev ev) = ev

instance ( Monad m, EventProbabilityCache m cacheP cachePC ev
         , AtomicEventDomain Event ev, Ord ev ) =>

    ProbabilityEval m cacheP cachePC ev where

    tryEvalProb pcache cpcache prob@(EvProb evp)

        | notCondProb evp = let Just (P ev _) = asProb evp
            in case lookupProbCache pcache ev of Just mp -> do p <- mp
                                                               return . EvProb $ ev ~~ p
                                                 _       -> return prob
        | otherwise = maybe (return prob) doUpd mbUpd
            where Just (PCond ev cond _) = asCondProb evp
                  mbUpd = do -- P(Y = y)
                             mpe <- lookupProbCache pcache ev
                             -- P(X.. | Y)
                             mpc <- lookupProbCache cpcache (cond, ev)

--                             atoms <- getAtoms ev
--                             let dys = Set.map (eventDomain . extractAtomEv) atoms
                             atoms' <- getAtoms ev
                             let [atoms] = Set.elems atoms'
                             let dys = Set.map Ev $ eventDomain atoms

                             mps <- sequence $ do
                                   y <- Set.toList dys
                                   return $ do -- P(Y = y')
                                               mpe' <- lookupProbCache pcache y
                                               -- P(X.. | Y = y')
                                               mpc' <- lookupProbCache cpcache (cond, y)
                                               return (mpe', mpc')

                             return (mpe, mpc, mps)

                  doUpd (mpe, mpc, mps) = do
                        pe <- mpe
                        pc <- mpc

                        let f (mpe', mpc') = do pe' <- mpe'
                                                pc' <- mpc'
                                                return $ pe' * pc'
                        ps <- mapM f mps

                        let prob = pe * pc * sum ps

                        return .EvProb $ ev ~| cond ~~ prob




