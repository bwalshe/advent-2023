{-# LANGUAGE OverloadedStrings #-}

module TestDay3 (tests) where

import qualified Data.Set as S
import Day3
import Test.HUnit

testReadPart :: Test
testReadPart =
  let emptyCase = assertEqual "Empty" (readPart "as;lkjdh") Nothing
      numberOnOwn =
        assertEqual
          "Just a number"
          (Just (Part 123 0 3, ""))
          (readPart "123")
      numberWithLeadAndFollow =
        assertEqual
          "leading and following chars"
          (Just (Part 123 3 6, "..."))
          (readPart "...123...")
   in TestCase $ emptyCase <> numberOnOwn <> numberWithLeadAndFollow

testReadMultipleParts =
  TestCase $
    assertEqual
      "two parts"
      [Part 123 3 6, Part 456 9 12]
      (readParts "...123...456...")

testSymbolLocations :: Test
testSymbolLocations =
  TestCase $
    assertEqual
      "positions"
      (S.fromAscList [1, 4])
      (symbolLocations ".%.1*..")

testPad :: Test
testPad =
  TestCase $
    assertEqual
      "padd above and below with a line of ...s"
      ["...", "aaa", "aaa", "..."]
      (pad ["aaa", "aaa"])

testSliding :: Test
testSliding =
  TestCase $
    assertEqual
      "Sliding window of 3 elements"
      [[0 .. 2], [1 .. 3], [2 .. 4], [3 .. 5], [4 .. 6]]
      (sliding 3 [0 .. 6])

testSumParts :: Test
testSumParts =
  let corners =
        assertEqual
          "SumParts find things at corners"
          [1, 1]
          (sumParts <$> [["*..", ".1.", "..."], ["*..", ".1.", "..."]])
      outOfBounds =
        assertEqual
          "sumParts checks correct bounds"
          0
          (sumParts [".....", "*.1.*", "....."])
   in TestCase $ corners <> outOfBounds

tests :: Test
tests =
  TestList
    [ testReadPart,
      testReadMultipleParts,
      testSymbolLocations,
      testPad,
      testSliding,
      testSumParts
    ]
