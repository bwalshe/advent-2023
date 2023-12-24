{-# LANGUAGE TupleSections #-}

module Day7 (Card (..), Score (..), Hand, toCard, toHand, scoreHand, scoreRound, task1) where

import Data.List (sort, sortOn)
import Data.Map.Strict (Map, fromListWith, toDescList)
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

toHand :: Text -> Hand
toHand h = Hand $ toCard <$> unpack h

scoreHand :: Hand -> Score
scoreHand (Hand h) =
  case reverse $ sort $ snd <$> toDescList (fromListWith (+) $ fmap (,1) h) of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    (3 : _) -> ThreeOfAKind
    (2 : 2 : _) -> TwoPair
    (2 : _) -> Pair
    (1 : _) -> Single

-- scoreRound :: [(Hand, Int)] -> Int
scoreRound r =
  let ordering (h, _) = (scoreHand h, h)
      ranked = snd <$> sortOn ordering r
   in sum $ uncurry (*) <$> zip [1 ..] ranked

parseLine :: Text -> (Hand, Int)
parseLine l = (\[h, b] -> (toHand h, read $ unpack b)) $ split (' ' ==) l

task1 :: String -> Text -> Either Text Int
task1 f t = Right $ scoreRound $ parseLine <$> Data.Text.lines t
