{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Applicative (asum)
import Control.Arrow (left)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (eol, hspace1, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

-------------------------------------------------------------------------------
-- Datatypes                                                                 --
-------------------------------------------------------------------------------

data Mapping = Mapping
  { destStart :: Int,
    sourceStart :: Int,
    rangeLen :: Int
  }
  deriving (Show)

type MappingSection = [Mapping]

type Alminac = [MappingSection]

type Seeds = [Int]

-------------------------------------------------------------------------------
-- Parsing                                                                   --
-------------------------------------------------------------------------------

type Parser = Parsec Void Text

pSeeds :: Parser Seeds
pSeeds = label "seeds" $ string "seeds:" *> many (hspace1 *> decimal) <* eol

pMapping :: Parser Mapping
pMapping = label "mapping" $ do
  d <- decimal
  hspace1
  s <- decimal
  hspace1
  r <- decimal
  eol
  return $ Mapping d s r

pSection :: Text -> Parser MappingSection
pSection name = label "section" $ string name *> string " map:" *> eol *> many pMapping

pAliminac = label "Alminac" $ do
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
  return
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

parseFile :: String -> Text -> Either Text (Seeds, Alminac)
parseFile =
  runParserSimple
    ( do
        seeds <- pSeeds
        eol
        -- eol
        alminac <- pAliminac
        return (seeds, alminac)
    )

-------------------------------------------------------------------------------
-- Task                                                                      --
-------------------------------------------------------------------------------
mapVal :: [Mapping] -> Int -> Int
mapVal mappings i =
  let mapSingle m =
        let s = sourceStart m
            e = s + rangeLen m
         in if i >= s && i < e
              then Just $ i - s + destStart m
              else Nothing
   in fromMaybe i $ asum $ mapSingle <$> mappings

makeMappings :: Alminac -> [Int -> Int]
makeMappings = fmap mapVal

makeSeedToLocation :: Alminac -> Int -> Int
makeSeedToLocation = foldl (.) id . reverse . makeMappings

expandRanges :: Seeds -> Seeds -> Seeds
expandRanges acc [] = acc
expandRanges acc (s : r : tail) = [s .. s + r]

task1 :: String -> Text -> Either Text Int
task1 f t = (\(s, a) -> minimum $ makeSeedToLocation a <$> s) <$> parseFile f t

task2 :: String -> Text -> Either Text Int
task2 f t = (\(s, a) -> minimum $ makeSeedToLocation a <$> expandRanges [] s) <$> parseFile f t
