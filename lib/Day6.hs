{-# LANGUAGE OverloadedStrings #-}

module Day6 (gameBounds, waysToWin, task1) where

import Control.Arrow (left)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (eol, hspace1, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

-------------------------------------------------------------------------------
-- Parsing                                                                   --
-------------------------------------------------------------------------------

type Parser = Parsec Void Text

pTimes :: Parser [Int]
pTimes = label "Time" $ string "Time:" *> many (hspace1 *> decimal) <* eol

runParserSimple :: Parser a -> String -> Text -> Either Text a
runParserSimple p f t = left (pack . errorBundlePretty) (runParser p f t)

pDistances :: Parser [Int]
pDistances = label "Distance" $ string "Distance:" *> many (hspace1 *> decimal) <* eol

parseFile :: String -> Text -> Either Text ([Int], [Int])
parseFile =
  runParserSimple
    ( do
        times <- pTimes
        distances <- pDistances
        return (times, distances)
    )

-------------------------------------------------------------------------------
-- Task                                                                      --
-------------------------------------------------------------------------------
{-
 - x * (t-x) = d + 1
 - x^2 - tx + d+1 = 0
 - t +/- sqrt(t^2 -4(d+1)) / 2
-}

gameBounds :: Int -> Int -> (Int, Int)
gameBounds t d =
  let delta = sqrt $ fromIntegral $ t * t - 4 * (d + 1)
      bound x = (fromIntegral t + x) / 2
   in (ceiling $ bound $ -1.0 * delta, floor $ bound delta)

waysToWin :: (Int, Int) -> Int
waysToWin (l, h) = h - l + 1

task1 :: String -> Text -> Either Text Int
task1 f t = do
  (ts, ds) <- parseFile f t
  let countWays = waysToWin . uncurry gameBounds
  return $ product $ countWays <$> zip ts ds

{- No `task2` today, as it was easy enough to do in the REPL -}
