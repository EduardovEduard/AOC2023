module Day3 where

import Data.Char (isDigit, digitToInt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, unions, (!), lookup)
import Control.Monad.State (State, get, put, modify, execState, when, unless)
import Data.Maybe (mapMaybe)

type Coordinates = (Int, Int)
type Indices = [Coordinates]
type CoordinateIdMap = Map Coordinates Int
type IdNumberMap = Map Int Int
type MyState = (Int, Indices, CoordinateIdMap, Indices, Int, IdNumberMap)

data PartMap = PartMap { partMap :: CoordinateIdMap, parts :: Indices, numberIds :: IdNumberMap }

isPart :: Char -> Bool
isPart c = not (isDigit c) && c /= '.'

flushMap :: State MyState ()
flushMap = do
    (num, coords, resultMap, parts, currentId, idMap) <- get
    let updateMap = foldl (\m i -> Map.insert i currentId m) resultMap coords
    let updateIdMap = if null coords then idMap else Map.insert currentId num idMap
    put (0, [], updateMap, parts, currentId + 1, updateIdMap)

processChar :: Int -> Char -> Int -> State MyState ()
processChar lineId c idx
    | isDigit c = do
        let digit = digitToInt c
        (num, coords, resultMap, parts, currentId, idMap) <- get
        put (10 * num + digit, (lineId, idx) : coords, resultMap, parts, currentId, idMap)
    | otherwise = do
        flushMap
        when (isPart c) $ modify (\(num, indices, resultMap, parts, currentId, idMap) -> (num, indices, resultMap, (lineId, idx) : parts, currentId, idMap))

-- Lineid * 1000 hax not to introduce another global counter
processLine :: Int -> String -> PartMap
processLine lineId str =
    let (current, coords, resultMap, parts, currentId, idMap) = execState (processAllChars str) (0, [], Map.empty, [], lineId * 1000, Map.empty)
        in PartMap resultMap parts idMap
    where
        processAllChars :: String -> State MyState ()
        processAllChars s = do
            mapM_ (uncurry (processChar lineId)) (zip s [0..])
            flushMap

neighbours :: Coordinates -> [Coordinates]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- returns the list of number ids adjacent to the coordinates of the part
processPart :: Coordinates -> CoordinateIdMap -> [Int]
processPart c numMap = mapMaybe (`Data.Map.lookup` numMap) (neighbours c)

day3a :: String -> Int
day3a content = let
        states = uncurry processLine <$> zip [0..] (lines content)
        numberCoordinates = unions $ partMap <$> states
        partCoordinates = concatMap parts states
        idMap = unions $ numberIds <$> states
        allNeighbours = unique $ concatMap (`processPart` numberCoordinates) partCoordinates
    in sum $ map (idMap !) allNeighbours


gearRatio :: Coordinates -> CoordinateIdMap -> IdNumberMap -> Int
gearRatio c numMap idMap = let
    neighbourIds = unique $ processPart c numMap
    in if length neighbourIds == 2 then (idMap ! head neighbourIds) * (idMap ! head (tail neighbourIds)) else 0


day3b :: String -> Int
day3b content = let
    states = uncurry processLine <$> zip [0..] (lines content)
    numberCoordinates = unions $ partMap <$> states
    partCoordinates = concatMap parts states
    idMap = unions $ numberIds <$> states
    in sum $ map (\c -> gearRatio c numberCoordinates idMap) partCoordinates