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

import System.Environment
import System.Exit

import WekaData.Show.Name

-----------------------------------------------------------------------------

main = getArgs >>= parse

parse ["-h"]  = usage         >> exitSuccess
parse [fname] = runWeka fname >> exitSuccess

parse _ = unknownCmd >> usage >> exitFailure




unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: NaiveBayesWeka [-h] file"
           putStrLn "       where file is an *.arff nominal data file" -- TODO: numerics!


