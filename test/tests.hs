module Main (
    main
) where

import Test.Hspec

import qualified ProbabilitySpec as PSpec

main :: IO ()
main = hspec $ do PSpec.spec

