module Main where

import Data.Int (Int)
import qualified System.Exit as Exit
import Test.HUnit
import qualified TestDay1
import qualified TestDay3
import qualified TestDay4
import qualified TestDay5

runMultiple :: [Test] -> IO [Counts]
runMultiple = mapM runTestTT

countFailures :: [Counts] -> Int
countFailures = sum . fmap failures

main :: IO ()
main = do
  results <-
    runMultiple
      [ TestDay1.tests,
        TestDay3.tests,
        TestDay4.tests,
        TestDay5.tests
      ]
  if countFailures results > 0 then Exit.exitFailure else Exit.exitSuccess
