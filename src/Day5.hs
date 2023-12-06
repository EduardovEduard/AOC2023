module Day5 where

import Data.Char (isDigit, digitToInt, isSpace)
import Data.Map (Map, unions, (!), lookup)
import Data.List(sortOn)
import qualified Data.Map as Map

type InputRange = (Int, Int, Int)

data InputData = InputData {
    seeds :: [Int],
    seedToSoilMap :: [InputRange],
    soilToFertilizerMap :: [InputRange],
    fertilizerToWaterMap :: [InputRange],
    waterToLightMap :: [InputRange],
    lightToTemperatureMap :: [InputRange],
    temperatureToHumidityMap :: [InputRange],
    humidityToLocationMap :: [InputRange]
}

type Interval = (Int, Int)
type IdRange = [(Interval, Interval)]

sortedRanges :: [InputRange] -> [InputRange]
sortedRanges = sortOn (\(_, b, _) -> b)

mapping :: IdRange -> Int -> Int
mapping [] i = i
mapping (((dstart, dend), (sstart, send)):xs) id
    | id >= sstart && id <= send = (id - sstart) + dstart
    | otherwise = mapping xs id


groupInput :: [String] -> [[String]]
groupInput = foldr helper [[]]
    where
        helper "" ([]:acc) = []:acc
        helper "" acc      = []:acc
        helper s (x:xs)    = (s:x):xs
        helper _ []        = error "impossible"

stripHeaders :: [[String]] -> [[String]]
stripHeaders (list) = [
    [drop 7 $ head (list !! 0)],
    drop 1 (list !! 1),
    drop 1 (list !! 2),
    drop 1 (list !! 3),
    drop 1 (list !! 4),
    drop 1 (list !! 5),
    drop 1 (list !! 6),
    drop 1 (list !! 7)]


makeIdRange :: [InputRange] -> IdRange
makeIdRange [] = []
makeIdRange (x:xs) = let
    (dstart, sstart, size) = x
    in ((dstart, dstart + size), (sstart, sstart + size)) : makeIdRange xs

makeRanges :: InputData -> ([Int], [IdRange])
makeRanges d = let
    seedToSoil = sortedRanges $ seedToSoilMap d
    soilToFertilizer = sortedRanges $ soilToFertilizerMap d
    fertilizerToWater = sortedRanges $ fertilizerToWaterMap d
    waterToLight = sortedRanges $ waterToLightMap d
    lightToTemperature = sortedRanges $ lightToTemperatureMap d
    temperatureToHumidity = sortedRanges $ temperatureToHumidityMap d
    humidityToLocation = sortedRanges $ humidityToLocationMap d
    in (seeds d, [makeIdRange seedToSoil, makeIdRange soilToFertilizer, makeIdRange fertilizerToWater, makeIdRange waterToLight, makeIdRange lightToTemperature, makeIdRange temperatureToHumidity, makeIdRange humidityToLocation])


readInputRange :: String -> InputRange
readInputRange str = (read $ nums !! 0, read $ nums !! 1, read $ nums !! 2)
    where nums = words str

readData :: String -> InputData
readData content = let
    numberStrings = stripHeaders . groupInput . lines $ content
    in InputData (map read $ words $ head (numberStrings !! 0)) (map readInputRange (numberStrings !! 1)) (map readInputRange (numberStrings !! 2)) (map readInputRange (numberStrings !! 3)) (map readInputRange (numberStrings !! 4)) (map readInputRange (numberStrings !! 5)) (map readInputRange (numberStrings !! 6)) (map readInputRange (numberStrings !! 7))

getLocationId :: [IdRange] -> Int -> Int
getLocationId xs id = foldl (flip mapping) id xs

expandSeedIds :: [Int] -> [Int]
expandSeedIds [] = []
expandSeedIds (x:xs) = [x + i | i <- [0..head xs-1]] ++ expandSeedIds (tail xs)

day5a :: String -> Int
day5a content = let
    d = readData content
    (seedIds, ranges) = makeRanges d
    in minimum $ map (getLocationId ranges) seedIds

day5b :: String -> Int
day5b content = 0