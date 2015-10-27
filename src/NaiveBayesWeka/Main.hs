-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

module Main ( main ) where

import NaiveBayes.Discrete.Weka
--import Event.Probability.Eval.Impl

import System.Environment
import System.Exit

import Data.Maybe (fromMaybe)
import Data.IORef

import qualified Data.Set as Set

import WekaData
import WekaData.Show.Name

-----------------------------------------------------------------------------

main = getArgs >>= parse

parse ["-h"]  = usage         >> exitSuccess
-- TODO: temp
parse [fname] = do
    (rawData, caches) <- runWeka fname

    let wattrs = rwdAttrs rawData
    let toW = fromMaybe (error "not an attribute") . uncurry (toWekaVal wattrs)

    let clazz = ("play", "no")
    let attrs = [ ("humidity", "high")
                , ("outlook" , "rainy")
                , ("temperature", "mild")
                , ("windy", "TRUE")
                ]

    res <- askPC caches (toW clazz) (Set.fromList $ map toW attrs)

    putStrLn "Res"
    print res

    printCache caches
    putStrLn ""

parse _ = unknownCmd >> usage >> exitFailure


printCache caches = do
    putStrLn ""
    putStrLn "Probabilities:\n"
    readIORef (probCache caches) >>= printMutMap "p"

    putStrLn ""
    putStrLn "Conditional Probabilities:\n"
    readIORef (condProbCache caches) >>= printMutMap "p"


--parse [fname] = runWeka fname >> exitSuccess

--parse ([fname, classAttr, classVal]:cond) =


unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: NaiveBayesWeka [-h] file"
           putStrLn "       where file is an *.arff nominal data file" -- TODO: numerics!


--askPC
