{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day1
import qualified System.Exit as Exit
import Test.HUnit

testExtract :: Test
testExtract = TestCase (assertEqual "should return 12 from \"sd1vonsvj4alsn2askn\"" 12 (extractNum "sd1vonsvj4alsn2askn"))

testNormalise :: Test
testNormalise = TestCase (assertEqual "should return 12 from \"oneight2\"" 12 (extractNum $ normalise "oneight2"))

tests :: Test
tests =
  TestList
    [ TestLabel "testExtract" testExtract,
      TestLabel "testNormalise" testNormalise
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
