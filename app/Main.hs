module Main where

import qualified Data.Text.IO as TIO
import qualified Day1
import System.Environment

main :: IO ()
main = do
  fileName <- head <$> getArgs
  fileContents <- TIO.readFile fileName
  print $ Day1.secondTask fileContents
