module Main where

import qualified Data.Text.IO as TIO
import qualified Day3
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  fileName <- Prelude.head <$> getArgs
  fileText <- TIO.readFile fileName
  print $ Day3.task2 fileText
