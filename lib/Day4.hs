{-# LANGUAGE OverloadedStrings #-}

module Day4 (Card (..), pNumList, pCard, task1, task2, findAllCopies) where

import Data.List (intersect, sortOn)
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Util

-------------------------------------------------------------------------------
-- Datatypes                                                                 --
-------------------------------------------------------------------------------

data Card = Card
  { num :: Int,
    winning :: [Int],
    actual :: [Int]
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Parsing                                                                   --
-------------------------------------------------------------------------------

pNumList :: Parser [Int]
pNumList = space *> some (L.decimal <* space)

pCard :: Parser Card
pCard = do
  string' "Card"
  space
  n <- L.decimal
  char ':'
  winning <- pNumList
  char '|'
  Card n winning <$> pNumList

parseCards :: String -> Text -> Either Text [Card]
parseCards = runParserSimple (many pCard)

-------------------------------------------------------------------------------
-- functions for task                                                        --
-------------------------------------------------------------------------------

countMatches :: Card -> Int
countMatches (Card _ l1 l2) = length $ intersect l1 l2

scoreCard :: Card -> Int
scoreCard c = case countMatches c of
  0 -> 0
  l -> 2 ^ (l - 1)

findAllCopies :: V.Vector [Int] -> [Int] -> [Int]
findAllCopies table = concatMap (table V.!)

countTotalCopies :: [Card] -> Int
countTotalCopies cards =
  let nextIndexes c@(Card n l1 l2) = [n .. n + countMatches c - 1]
      table = V.fromList $ nextIndexes <$> cards
      countTotalCopies' acc current = case current of
        [] -> acc
        _ -> countTotalCopies' (acc + length current) $ findAllCopies table current
   in countTotalCopies' 0 [0 .. length cards - 1]

runTask :: ([Card] -> Int) -> String -> Text -> Either Text Int
runTask f name text = f <$> parseCards name text

task1 :: Task Int
task1 = runTask (sum . fmap scoreCard)

task2 :: Task Int
task2 = runTask countTotalCopies
