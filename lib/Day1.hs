{-# LANGUAGE OverloadedStrings #-}

module Day1 (extractNum, normalise, firstTask, secondTask) where

import Data.Char
import Data.Int (Int)
import Data.Text (Text, pack, replace)
import qualified Data.Text as T

extractNum :: Text -> Int
extractNum s =
  let nums = T.filter isDigit s
   in read $ [T.head, T.last] <*> [nums]

normalise :: Text -> Text
normalise =
  let words = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      makePair (w, n) = (w, w <> pack (show n) <> w)
      pairs = makePair <$> zip words [1 ..]
   in foldr (.) id $ uncurry replace <$> pairs

runAndSum :: (Text -> Int) -> Text -> Int
runAndSum f = sum . fmap f . T.lines

firstTask :: Text -> Int
firstTask = runAndSum extractNum

secondTask :: Text -> Int
secondTask = runAndSum (extractNum . normalise)
