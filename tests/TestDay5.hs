{-# LANGUAGE OverloadedStrings #-}

module TestDay5 (tests) where

import Data.Either (fromRight)
import Data.List (sortOn)
import Day5
import Test.HUnit
import Text.ParserCombinators.ReadP (many1)

testMappingConstruction :: Test
testMappingConstruction =
  let goodInput = [((1, 2), 1), ((2, 3), 2)]
      badInput = [((1, 3), 1), ((2, 4), 2)]
   in TestCase $ do
        assertEqual
          "intervals do not overlap"
          (Right goodInput)
          (getIntervals <$> mappingFromList goodInput)
        assertEqual
          "order does not matter"
          (Right goodInput)
          (getIntervals <$> mappingFromList (reverse goodInput))
        assertEqual
          "intervals overlap"
          (Left "overlap")
          (mappingFromList badInput)

testMonoid :: Test
testMonoid =
  let m1 = Mapping [((50, 98), 2), ((98, 100), -48)]
   in TestCase $ do
        assertEqual
          "mempty does nothing from the left"
          m1
          (mempty <> m1)
        assertEqual
          "mempty does noting from the right"
          m1
          (m1 <> mempty)

testStackMapping :: Test
testStackMapping =
  let m1 = Mapping [((50, 98), 2), ((98, 100), -48)]
      m2 = Mapping [((0, 15), 39), ((15, 54), -15)]
   in TestCase $
        assertEqual
          "stack with overlap"
          (mapVal m2 . mapVal m1 <$> [0 .. 100])
          (mapVal (m1 <> m2) <$> [0 .. 100])

tests :: Test
tests = TestList [testMappingConstruction, testMonoid, testStackMapping]
