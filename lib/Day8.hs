{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Arrow (left)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Node = Text

type Network = Map.Map Node (Node, Node)

data Direction = L | R deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------
type Parser = Parsec Void Text

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

runParserSimple :: Parser a -> String -> Text -> Either Text a
runParserSimple p f t = left (pack . errorBundlePretty) (runParser p f t)

parseFile :: String -> Text -> Either Text ([Direction], Network)
parseFile = runParserSimple pFile

-------------------------------------------------------------------------------
-- Task
-------------------------------------------------------------------------------
follow :: Network -> [Direction] -> Maybe Int
follow m = follow' m "AAA" 1
  where
    follow' _ _ _ [] = Nothing
    follow' m p c (h : t) = case pick h <$> Map.lookup p m of
      Nothing -> Nothing
      Just "ZZZ" -> Just c
      Just p' -> follow' m p' (c + 1) t
      where
        pick L = fst
        pick R = snd

task1 :: String -> Text -> Either Text (Maybe Int)
task1 f t = do
  (directions, network) <- parseFile f t
  return $ follow network $ concat $ repeat directions
