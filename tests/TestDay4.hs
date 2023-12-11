{-# LANGUAGE OverloadedStrings #-}

module TestDay4 where

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

tests :: Test
tests =
  TestList
    [ testList,
      testParseCard
    ]
