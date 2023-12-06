module Main where

import qualified Data.Text.IO as TIO
import qualified Day2
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  fileName <- Prelude.head <$> getArgs
  fileText <- TIO.readFile fileName
  case Day2.task2 fileName fileText of
    Left e -> putStrLn $ errorBundlePretty e
    Right count -> print count
