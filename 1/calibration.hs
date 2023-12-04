{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text (Text, pack, replace)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment
import Text.Show

extractNum :: Text -> Int
extractNum s =
  let nums = T.filter isDigit s
   in read $ [T.head, T.last] <*> [nums]

sumCodes :: Text -> Int
sumCodes = sum . fmap (extractNum . normalise) . T.lines

normalise :: Text -> Text
normalise =
  let words = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      makePair (w, n) = (w, w <> pack (show n) <> w)
      pairs = makePair <$> zip words [1 ..]
   in foldr (.) id $ fmap (uncurry replace) pairs

main :: IO ()
main = do
  (fileName : _) <- getArgs
  rawData <- TIO.readFile fileName
  print $ sum $ extractNum . normalise <$> T.lines rawData
