-- |
--
-- Module      :  NaiveBayesWeka:Main
-- Description :  Main module of the __NaiveBayesWeka__ executable.
-- License     :  MIT
--
-- Main module of the __NaiveBayesWeka__ executable.
--
--
-- Command line usage:
--
--
--  NaiveBayesWeka [-h] learn-file test-file class-name
--
--
--      learn-file   | *.arff nominal data file, containing the learning data
--
--      test-file    | *.arff nominal data file, containing the testing data
--
--      class-name   | name of the attribute to classify by


module Main ( main ) where

import EventProbability.NaiveBayes.Weka

import System.Environment
import System.Exit

-----------------------------------------------------------------------------

import EventProbability.DefaultImpl

import WekaData.Show.Name

-----------------------------------------------------------------------------

main = getArgs >>= parse

parse ["-h"] = usage >> exitSuccess

parse [learnf, testf, clazz] = runWekaLearnAndTest learnf testf clazz True


parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: NaiveBayesWeka [-h] learn-file test-file class-name\n"
           putStrLn "       learn-file   | *.arff nominal data file, containing the learning data" -- TODO: numerics!
           putStrLn "       test-file    | *.arff nominal data file, containing the testing data" -- TODO: numerics!
           putStrLn "       class-name   | name of the attribute to classify by"

