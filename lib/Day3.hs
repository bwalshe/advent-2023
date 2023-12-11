{-# LANGUAGE OverloadedStrings #-}

module Day3 (Part (..), readPart, readParts, sumParts, findRatios, symbolLocations, pad, sliding, task1, task2) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T

data Part = Part
  { number :: Int,
    start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

type Ratio = (Int, Int)

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

symbolLocations :: T.Text -> [Int]
symbolLocations line =
  let indexed = zip (T.unpack line) [0 ..]
   in snd <$> filter (\(c, i) -> not (isDigit c || c == '.')) indexed

pad :: [T.Text] -> [T.Text]
pad lines =
  let w = T.length $ head lines
   in T.pack (replicate w '.') : lines ++ [T.pack $ replicate w '.']

sliding :: Int -> [a] -> [[a]]
sliding n xs =
  if length xs < n
    then []
    else take n xs : sliding n (tail xs)

isValidPart :: S.Set Int -> Part -> Bool
isValidPart s p = not $ S.disjoint s $ S.fromAscList [start p - 1 .. end p]

sumParts :: [T.Text] -> Int
sumParts [top, middle, bottom] =
  let parts = readParts middle
      symbols = S.unions $ S.fromAscList . symbolLocations <$> [top, middle, bottom]
   in sum $ number <$> filter (isValidPart symbols) parts

findRatios :: [T.Text] -> [Ratio]
findRatios [top, middle, bottom] =
  let symbols = symbolLocations middle
      parts = concatMap readParts [top, middle, bottom]
      findMatches i = filter (\(Part _ s e) -> i >= s - 1 && i <= e) parts
      pickPair l = case l of
        [p1, p2] -> Just (number p1, number p2)
        _ -> Nothing
   in mapMaybe (pickPair . findMatches) symbols

task1 :: T.Text -> Int
task1 raw = sum $ sumParts <$> sliding 3 (pad $ T.lines raw)

task2 :: T.Text -> Int
task2 raw =
  let sumRatios t = sum $ uncurry (*) <$> findRatios t
   in sum $ sumRatios <$> sliding 3 (pad $ T.lines raw)
