{-# LANGUAGE OverloadedStrings #-}

module TestDay4 where

import Data.List
import Data.Map (fromList)
import qualified Data.Vector as V
import Day4
import Test.HUnit
import Text.Megaparsec

testList :: Test
testList =
  TestCase $
    assertEqual "asdf" (Right [1, 2, 3]) (runParser pNumList "test" " 1 2 3 ")

testParseCard :: Test
testParseCard =
  TestCase $
    assertEqual
      "Parse Card"
      (Right $ Card 1 [1, 2] [3, 4, 5])
      (runParser pCard "test" "Card 1: 1 2 | 3 4 5")

testAllCopies :: Test
testAllCopies =
  let indexTable =
        V.fromList
          [ [],
            [2, 3],
            [3, 4, 5],
            [],
            [],
            []
          ]
   in TestCase $
        assertEqual
          "find all copies"
          [2, 3, 3, 4, 5]
          (sort $ findAllCopies indexTable [0 .. 5])

tests :: Test
tests =
  TestList
    [ testList,
      testParseCard,
      testAllCopies
    ]
