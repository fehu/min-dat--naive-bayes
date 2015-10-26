--
--
-- Module      :  ProbabilitySpec
-- License     :  MIT
-- Stability   :  dev
--
--

module ProbabilitySpec (spec) where

import Event.Probability

import Test.Hspec

spec :: Spec
spec = do

    let a = Ev 'a'
    let b = Ev 'b'
    let c = Ev 'c'
    let d = Ev 'd'

    describe "Event" $ do

        specify "an atomic is compared by underlying label" $ example $ do
            Ev 1 `shouldBe`    Ev 1
            Ev 1 `shouldNotBe` Ev 2
            Ev [1,2] `shouldBe`    Ev [1,2]
            Ev [1,2] `shouldNotBe` Ev [2,1]


        describe "union" $ do
            specify "of atomic events" $
                    example $ c & a & c & b & c `shouldBe` a & b & c
            specify "of unions" $
                    example $ (a & b) & (a & c)  `shouldBe` a & b & c
            specify "with intersect" $ do

                    example $ (a # b) & a         `shouldBe` a
                    example $ (a # b) & d         `shouldBe` (a # b) & d

                    example $ (a # b) & (a # c)  `shouldBe` a # (b & c)
                    example $ (a # b) & (c # d)  `shouldBe` (a # b) & (c # d)

                    example $ (a # b) & (a & c)  `shouldBe` a & c
                    example $ (a # b) & (c & d)  `shouldBe` (a # b) & c & d

        describe "intersection" $ do
            specify "of atomic events" $
                    example $ c # a # c # b # c `shouldBe` a # b # c
            specify "of intersections" $
                    example $ (a # b) # (a # c)  `shouldBe` a # b # c
            specify "with union" $ do

                    example $ (a & b) # a         `shouldBe` a
                    example $ (a & b) # d         `shouldBe` d # (a & b)

                    example $ (a & b) # (a & c)  `shouldBe` a & (b # c)
                    example $ (a & b) # (c & d)  `shouldBe` (a & b) # (c & d)

                    example $ (a & b) # (a # c)  `shouldBe` a # c
                    example $ (a & b) # (c # d)  `shouldBe` c # d # (a & b)

        describe "complement" $ do
            specify "complement of complement" $ example $ (neg . neg) a `shouldBe` a
            specify "union" $ do
                    example $ neg a & neg b `shouldBe` neg (a # b)
                    example $ neg a & a     `shouldBe` Universal
            specify "intersection" $ do
                    example $ neg a # neg b `shouldBe` neg (a & b)
                    example $ neg a # a     `shouldBe` Null

        describe "Universal" $ do
            specify "complement"    $ example $ neg Universal   `shouldBe` (Null :: Event Char)
            specify "union"         $ example $ Universal & a   `shouldBe` Universal
            specify "intersection"  $ example $ Universal # a   `shouldBe` a

        describe "Null" $ do
            specify "complement"    $ example $ neg Null   `shouldBe` (Universal :: Event Char)
            specify "union"         $ example $ Null & a   `shouldBe` a
            specify "intersection"  $ example $ Null # a   `shouldBe` Null

--    describe "Probability" $ do
--        context "given precalculated cache" $
--            let pa = a ~~ 0.3
--            let pb
--
--            specify "" $ example undefined


--
--        describe "union: P(A) & P(B) -> P(A & B)" $
--            specify "P(A & B) = P(A) + P(B) - P(A # B)" $
--                    example $ shouldBe (pUnknown a & pUnknown b)
--                                       (pUnknown a & pUnknown b & neg (pUnknown (a#b)))
--
--        describe "intersection: P(A) & P(B) -> P(A & B)" $
--            specify "needs to be specified explicitly" $
--                    example $ ((a ~~ 0.2) # (b ~~ 0.3)) `shouldBe` pUnknown (a#b)
--
--        describe "complement: 'P(A) -> P('A)" $
--            specify "'P(A: p) = P('A: 1-p)" $
--                    example $ neg (a ~~ 0.2) `shouldBe` (neg a ~~ 0.8)

        specify "pending" pending



