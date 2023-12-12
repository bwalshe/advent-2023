module TestDay5 (tests) where

import Day5
import Test.HUnit

testMapping :: Test
testMapping =
  let single =
        assertEqual
          "mapVal works over s single range"
          [97, 50, 51, 100]
          ( mapVal [Mapping 50 98 2]
              <$> [97 .. 100]
          )
      multiple =
        assertEqual
          "mapVal works over multiple ranges"
          ([0 .. 49] ++ [52 .. 99] ++ [50, 51])
          (mapVal [Mapping 50 98 2, Mapping 52 50 48] <$> [0 .. 99])
   in TestCase $ single <> multiple

tests :: Test
tests = TestList [testMapping]
