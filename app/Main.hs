module Main where

import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import qualified Day8
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  fileName <- Prelude.head <$> getArgs
  fileText <- TIO.readFile fileName
  case Day8.task1 fileName fileText of
    Right answer -> print $ show answer
    Left e -> putStrLn $ unpack e
