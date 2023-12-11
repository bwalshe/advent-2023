{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( pColor,
    pCount,
    pDraw,
    pGame,
    testGame,
    testDraw,
    parseGames,
    sumBadGames,
    Count (..),
    Game (..),
    task1,
    task2,
  )
where

import Data.Text
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

-- The goal for today is to parse out information about draws of colored balls
-- from a bag, so that you can make estimates about how many balls area  actually
-- in the bag

type Parser = Parsec Void Text

-------------------------------------------------------------------------------
-- Data Structures                                                            -
-------------------------------------------------------------------------------

data Color = Red | Green | Blue deriving (Eq, Show)

-- This represents a sample taken from a bag with red, green and blue balls
-- They form a semigroup because you can add two Counts together
-- They form a monoid as the empty count exists
data Count = Count
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Eq, Show)

instance Semigroup Count where
  (Count r1 g1 b1) <> (Count r2 g2 b2) = Count (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Count where
  mempty = Count 0 0 0

-- A Game just consists of a list of draws from the bag
data Game = Game
  { number :: Int,
    draws :: [Count]
  }
  deriving (Show)

makeCount :: Color -> Int -> Count
makeCount Red n = Count n 0 0
makeCount Green n = Count 0 n 0
makeCount Blue n = Count 0 0 n

-------------------------------------------------------------------------------
-- Parsing                                                                   --
-------------------------------------------------------------------------------

pColor :: Parser Color
pColor =
  choice
    [ Red <$ string' "red",
      Green <$ string' "green",
      Blue <$ string' "blue"
    ]

pCount :: Parser Count
pCount = do
  n <- L.decimal
  _ <- space1
  color <- pColor
  return $ makeCount color n

pDraw :: Parser Count
pDraw = mconcat <$> sepBy pCount (string ", ")

pGame :: Parser Game
pGame = do
  _ <- string' "Game "
  n <- L.decimal
  _ <- string' ": "
  draws <- sepBy pDraw (string "; ")
  return $ Game n draws

parseGames :: String -> Text -> Either (ParseErrorBundle Text Void) [Game]
parseGames = runParser (many (pGame <* char '\n') <* many newline)

-------------------------------------------------------------------------------
-- functions for solving the tasks                                           --
-- ----------------------------------------------------------------------------

testDraw :: Int -> Int -> Int -> Count -> Bool
testDraw r g b count = (red count <= r) && (green count <= g) && (blue count <= b)

testGame :: Game -> Bool
testGame g = Prelude.all (testDraw 12 13 14) $ draws g

sumBadGames :: [Game] -> Int
sumBadGames = sum . fmap number . Prelude.filter testGame

maxCount :: Count -> Count -> Count
maxCount count1 count2 =
  let pickMax f = max (f count1) (f count2)
   in Count (pickMax red) (pickMax green) (pickMax blue)

sumMinPower :: [Game] -> Int
sumMinPower =
  let power (Count r g b) = r * g * b
   in sum . fmap (power . Prelude.foldr1 maxCount . draws)

task1 :: String -> Text -> Either (ParseErrorBundle Text Void) Int
task1 fileName contents = sumBadGames <$> parseGames fileName contents

task2 :: String -> Text -> Either (ParseErrorBundle Text Void) Int
task2 fileName contents = sumMinPower <$> parseGames fileName contents
