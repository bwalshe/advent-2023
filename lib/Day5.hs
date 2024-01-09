{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

import Control.Arrow (left)
import Control.Monad (foldM, join)
import Data.List (sortOn)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (eol, hspace1, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

-------------------------------------------------------------------------------
-- Datatypes                                                                 --
-------------------------------------------------------------------------------

type Seeds = [Int]

type Interval = (Int, Int)

type Shift = (Interval, Int)

newtype Mapping = Mapping
  { getIntervals :: [Shift]
  }
  deriving (Show, Eq)

instance Semigroup Mapping where
  (<>) = stackMapping

instance Monoid Mapping where
  mempty = Mapping []

translationAmount :: [Shift] -> Int -> Int
translationAmount m l =
  let overlap ((l1, h1), _) = l1 <= l && l < h1
   in case filter overlap m of
        [] -> 0
        [(_, n)] -> n
        wrong -> error $ "This should not be possible: " <> show l <> " " <> show wrong

mappingFromList :: [Shift] -> Either Text Mapping
mappingFromList shifts =
  let add [] s = Right [s]
      add l@(((l1, h1), v1) : t) s@((l2, h2), v2)
        | l2 > h2 = Left $ "invalid interval" <> pack (show (l2, h2))
        | h1 > l2 = Left "overlap"
        | h1 == l2 && v1 == v2 = Right $ ((l1, h2), v1) : t
        | h1 /= l2 = Right $ s : ((h1, l2), 0) : l
        | otherwise = Right $ s : l
   in fmap (Mapping . reverse) (foldM add [] (sortOn (fst . fst) shifts))

{- stack mappings so that we get a new mapping that acts like values have been passed through
 - the first mapping and then the second mapping
 -}
stackMapping :: Mapping -> Mapping -> Mapping
stackMapping (Mapping m1) (Mapping m2) =
  let em1 = extendBounds m1 $ mappingBounds (Mapping m2)
      em2 = extendBounds m2 $ mappingBounds (Mapping m1)
   in Mapping $ sortOn (fst . fst) $ concatMap (splitShift (Mapping em2)) $ getIntervals (Mapping em1)

splitShift :: Mapping -> Shift -> [Shift]
splitShift (Mapping m) ((l, h), t) =
  let l' = l + t
      h' = h + t
      inside i = l' < i && i < h'
      points' = l' : filter inside (fmap (fst . fst) m) ++ [h']
      points = fmap ((-t) +) points'
      makeIntervals pst = zip pst (tail pst)
      intervals = zip (makeIntervals points) (makeIntervals points')
      newSplit (i1, i2) = (i1, t + translationAmount m (fst i2))
   in newSplit <$> intervals

lowestIntervals :: Mapping -> [Interval]
lowestIntervals (Mapping m) =
  let imgMin ((l, _), t) = l + t
   in fst <$> sortOn imgMin m

mappingBounds :: Mapping -> Maybe Interval
mappingBounds (Mapping []) = Nothing
mappingBounds (Mapping m) = Just (fst $ fst $ head m, snd $ fst $ last m)

extendBounds :: [Shift] -> Maybe Interval -> [Shift]
extendBounds m Nothing = m
extendBounds [] (Just i) = [(i, 0)]
extendBounds m1 (Just (l, h)) =
  case mappingBounds (Mapping m1) of
    Just (l1, h1) ->
      ( let addHead m = if l < l1 then ((l, l1), 0) : m else m
            addTail m = if h > h1 then m ++ [((h1, h), 0)] else m
         in addHead $ addTail m1
      )
    _ -> m1

-------------------------------------------------------------------------------
-- Parsing                                                                   --
-------------------------------------------------------------------------------

type Parser = Parsec Void Text

pSeeds :: Parser Seeds
pSeeds = label "seeds" $ string "seeds:" *> many (hspace1 *> decimal) <* eol

pMapping :: Parser Shift
pMapping = label "mapping" $ do
  d <- decimal
  hspace1
  s <- decimal
  hspace1
  r <- decimal
  eol
  return ((s, s + r), d - s)

pSection :: Text -> Parser (Either Text Mapping)
pSection name = label "section" $ do
  string name
  string " map:"
  eol
  left (<> " in " <> name) . mappingFromList <$> many pMapping

pAlminac :: Parser (Either Text [Mapping])
pAlminac = label "Alminac" $ do
  seedToSoil <- pSection "seed-to-soil"
  space
  soilToFertilizer <- pSection "soil-to-fertilizer"
  space
  fertilizerToWater <- pSection "fertilizer-to-water"
  space
  waterToLight <- pSection "water-to-light"
  space
  lightToTemperature <- pSection "light-to-temperature"
  space
  temperatureToHumidity <- pSection "temperature-to-humidity"
  space
  humidityToLocation <- pSection "humidity-to-location"
  space
  return $
    sequence
      [ seedToSoil,
        soilToFertilizer,
        fertilizerToWater,
        waterToLight,
        lightToTemperature,
        temperatureToHumidity,
        humidityToLocation
      ]

runParserSimple :: Parser a -> String -> Text -> Either Text a
runParserSimple p f t = left (pack . errorBundlePretty) (runParser p f t)

parseFile :: String -> Text -> Either Text (Seeds, [Mapping])
parseFile f t =
  join $
    runParserSimple
      ( do
          seeds <- pSeeds
          eol
          fmap (seeds,) <$> pAlminac
      )
      f
      t

-------------------------------------------------------------------------------
-- Task                                                                      --
-------------------------------------------------------------------------------
mapVal :: Mapping -> Int -> Int
mapVal m i = case filter (\((l, h), _) -> l <= i && i < h) $ getIntervals m of
  [(_, s)] -> i + s
  [] -> i
  _ -> error "This shouldn't be possilbe"

makeRanges :: [Int] -> [Interval]
makeRanges [] = []
makeRanges (l : r : t) = (l, l + r) : makeRanges t

minimumMay :: (Ord a, Foldable f) => f a -> Maybe a
minimumMay l
  | null l = Nothing
  | otherwise = Just $ minimum l

tryMatch :: [Int] -> Interval -> Maybe Int
tryMatch seeds (l, h) = minimumMay $ filter (\s -> l <= s && s < h) seeds

minOverlap :: Interval -> Interval -> Maybe Int
minOverlap (l1, h1) (l2, h2)
  | l1 < h2 && h1 > l2 = Just $ max l1 l2
  | otherwise = Nothing

tryMatchInterval :: [Interval] -> Interval -> Maybe Int
tryMatchInterval seeds inter = sequence (filter (/= Nothing) $ minOverlap inter <$> seeds) >>= minimumMay

firstJust [] = Nothing
firstJust ((Just x) : t) = Just x
firstJust (_ : t) = firstJust t

findLowest :: [Int] -> [Interval] -> Maybe Int
findLowest seeds inters = firstJust $ tryMatch seeds <$> inters

findLowestInterval :: [Interval] -> [Interval] -> Maybe Int
findLowestInterval seeds inters = firstJust $ tryMatchInterval seeds <$> inters

task1 :: String -> Text -> Either Text (Maybe Int)
task1 f t = do
  (s, a) <- parseFile f t
  let m = mconcat a
  return $ mapVal m <$> findLowest s (lowestIntervals m)

task2 :: String -> Text -> Either Text (Maybe Int)
task2 f t = do
  (s, a) <- parseFile f t
  let m = mconcat a
  return $ mapVal m <$> findLowestInterval (makeRanges s) (lowestIntervals m)
