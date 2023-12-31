module Main where

import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import qualified Day8
import System.Environment (getArgs)
import Util

runTask :: (Show a) => Task a -> IO ()
runTask task = do
  fileName <- Prelude.head <$> getArgs
  fileText <- TIO.readFile fileName
  case task fileName fileText of
    Right answer -> print $ show answer
    Left e -> putStrLn $ unpack e

main :: IO ()
main = runTask Day8.task2
