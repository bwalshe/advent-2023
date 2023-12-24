{-# LANGUAGE TupleSections #-}

module Day7 (Card (..), Score (..), Hand, handToCount, toCard, toHand, scoreHand, scoreRound, task1, task2) where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, lines, pack, split, unpack)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Bounded, Enum, Show, Ord)

data Score = Single | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Bounded, Enum, Show)

newtype Hand = Hand [Card] deriving (Eq, Ord, Show)

toCard :: Char -> Card
toCard c = case c of
  'A' -> Ace
  'K' -> King
  'Q' -> Queen
  'J' -> Jack
  'T' -> Ten
  _ -> toEnum $ read [c] - 1

toCardJoker :: Char -> Card
toCardJoker c = case c of
  'A' -> Ace
  'K' -> King
  'Q' -> Queen
  'J' -> Joker
  'T' -> Ten
  _ -> toEnum $ read [c] - 1

toHand :: (Char -> Card) -> Text -> Hand
toHand f h = Hand $ f <$> unpack h

handToCount :: Hand -> Map.Map Card Int
handToCount (Hand h) = Map.fromListWith (+) $ fmap (,1) h

fixJoker :: Map.Map Card Int -> [Int]
fixJoker m =
  let addFront i [] = [i]
      addFront i (h : t) = (h + i) : t
      jokers = fromMaybe 0 $ Map.lookup Joker m
      m' = Map.delete Joker m
   in addFront jokers $ reverse $ sort $ snd <$> Map.toDescList m'

scoreHand :: Hand -> Score
scoreHand = countsToScore . fixJoker . handToCount

countsToScore :: [Int] -> Score
countsToScore l = case l of
  [5] -> FiveOfAKind
  [4, 1] -> FourOfAKind
  [3, 2] -> FullHouse
  (3 : _) -> ThreeOfAKind
  (2 : 2 : _) -> TwoPair
  (2 : _) -> Pair
  (1 : _) -> Single
  _ -> error $ show l

scoreRound :: [(Hand, Int)] -> Int
scoreRound r =
  let ordering (h, _) = (scoreHand h, h)
      ranked = snd <$> sortOn ordering r
   in sum $ uncurry (*) <$> zip [1 ..] ranked

parseLine :: (Char -> Card) -> Text -> (Hand, Int)
parseLine f = (\[h, b] -> (toHand f h, read $ unpack b)) . split (' ' ==)

task1 :: String -> Text -> Either Text Int
task1 f t = Right $ scoreRound $ parseLine toCard <$> Data.Text.lines t

task2 :: String -> Text -> Either Text Int
task2 f t = Right $ scoreRound $ parseLine toCardJoker <$> Data.Text.lines t
