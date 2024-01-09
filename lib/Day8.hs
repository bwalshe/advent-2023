{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Util

type Node = Text

type Network = Map.Map Node (Node, Node)

data Direction = L | R deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------
pDirections :: Parser [Direction]
pDirections = fmap (read . unpack) <$> many (string "L" <|> string "R")

pPair :: Parser (Text, Text)
pPair = do
  char '('
  hspace
  a <- some alphaNumChar
  hspace
  char ','
  hspace
  b <- some alphaNumChar
  hspace
  char ')'
  return (pack a, pack b)

pKeyVal :: Parser (Node, (Node, Node))
pKeyVal = do
  k <- some alphaNumChar
  hspace
  char '='
  hspace
  v <- pPair
  return (pack k, v)

pFile :: Parser ([Direction], Network)
pFile = do
  directions <- pDirections
  space
  network <- Map.fromList <$> many (pKeyVal <* eol)
  eof
  return (directions, network)

parseFile :: String -> Text -> Either Text ([Direction], Network)
parseFile = runParserSimple pFile

-------------------------------------------------------------------------------
-- Task
-------------------------------------------------------------------------------
pick :: Network -> Node -> Direction -> Maybe Node
pick net node d = pick' d <$> Map.lookup node net
  where
    pick' L = fst
    pick' R = snd

follow :: Network -> Node -> (Node -> Bool) -> [Direction] -> Maybe Int
follow m start endTest = follow' m start 1
  where
    follow' _ _ _ [] = Nothing
    follow' m p c (h : t) = case pick m p h of
      Nothing -> Nothing
      Just p' ->
        if endTest p'
          then Just c
          else follow' m p' (c + 1) t

task1 :: Task (Maybe Int)
task1 f t = do
  (directions, network) <- parseFile f t
  return $ follow network "AAA" (== "ZZZ") $ concat $ repeat directions

task2 :: Task (Maybe Int)
task2 f t = do
  (directions, network) <- parseFile f t
  let start = filter (\t -> Text.last t == 'A') $ Map.keys network
  let test t = Text.last t == 'Z'
  let repeating = concat $ repeat directions
  let distances = mapM (\s -> follow network s test repeating) start
  return $ foldl lcm 1 <$> distances
