{-# LANGUAGE OverloadedStrings #-}

module Day3 (Part (..), readPart, readParts, sumParts, symbolLocations, pad, sliding, task1) where

import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Text as T

data Part = Part
  { number :: Int,
    start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

readPart :: T.Text -> Maybe (Part, T.Text)
readPart s = case T.break isDigit s of
  (_, "") -> Nothing
  (lead, s') ->
    let (num, rest) = T.break (not . isDigit) s'
        offset = T.length lead
        end = offset + T.length num
     in Just (Part (read $ T.unpack num) offset end, rest)

readParts :: T.Text -> [Part]
readParts line =
  let shift n (Part num s e) = Part num (s + n) (e + n)
      readParts' acc offset s = case readPart s of
        Nothing -> acc
        Just (p, s') ->
          let p' = shift offset p
           in readParts' (p' : acc) (end p') s'
   in Prelude.reverse $ readParts' [] 0 line

symbolLocations :: T.Text -> S.Set Int
symbolLocations line =
  let indexed = zip (T.unpack line) [0 ..]
   in S.fromAscList $ snd <$> filter (\(c, i) -> not (isDigit c || c == '.')) indexed

pad :: [T.Text] -> [T.Text]
pad lines =
  let w = T.length $ head lines
   in T.pack (replicate w '.') : lines ++ [T.pack $ replicate w '.']

sliding :: Int -> [a] -> [[a]]
sliding n xs =
  if length xs < n
    then []
    else take n xs : sliding n (tail xs)

isValidPart :: Part -> S.Set Int -> Bool
isValidPart p s = not $ S.disjoint s $ S.fromAscList [start p - 1 .. end p]

sumParts :: [T.Text] -> Int
sumParts [top, middle, bottom] =
  let parts = readParts middle
      topSymbols = symbolLocations top
      middleSymbols = symbolLocations middle
      bottomSymbols = symbolLocations bottom
   in sum $ number <$> filter (\p -> isValidPart p topSymbols || isValidPart p middleSymbols || isValidPart p bottomSymbols) parts

task1 :: T.Text -> Int
task1 raw = sum $ sumParts <$> sliding 3 (pad $ T.lines raw)
