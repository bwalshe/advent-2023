{-# LANGUAGE OverloadedStrings #-}

module Day4 (Card (..), pNumList, pCard, task1) where

import Data.List (intersect)
import Data.Text
import Data.Text.Internal.Fusion.Types (RS (RS0))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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
  actual <- pNumList
  return $ Card n winning actual

parseCards :: String -> Text -> Either (ParseErrorBundle Text Void) [Card]
parseCards = runParser (many pCard)

-------------------------------------------------------------------------------
-- functions for task                                                        --
-------------------------------------------------------------------------------

scoreCard :: Card -> Int
scoreCard (Card _ w a) = case Prelude.length $ intersect w a of
  0 -> 0
  l -> 2 ^ (l - 1)

task1 :: String -> Text -> Either Text Text
task1 f t = case parseCards f t of
  Right cards -> Right $ pack $ show $ sum $ scoreCard <$> cards
  Left e -> Left $ pack $ errorBundlePretty e
