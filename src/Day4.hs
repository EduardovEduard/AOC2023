module Day4 where

import Data.Char (isDigit, digitToInt)
import Data.Set (fromList, member, intersection)
import Data.Map (Map, insert, findWithDefault, elems)
import qualified Data.Map as Map

cardValue :: String -> Int
cardValue str = let
    numString = tail $ dropWhile (/= ':') str
    (winning, draw) = splitOut (words numString) "|"
    winningSet = fromList $ map read winning
    drawNumbers = fromList $ map (read :: String -> Int) draw
    amount = length (intersection winningSet drawNumbers)
    in case amount of
        0 -> 0
        _ -> 2 ^ (amount - 1)

splitOut :: Eq a => [a] -> a -> ([a], [a])
splitOut [] _ = ([], [])
splitOut (x:xs) y = if x == y then ([], xs) else let (a, b) = splitOut xs y in (x:a, b)

day4a :: String -> Int
day4a content = let cards = lines content
    in sum $ map cardValue cards

nextCardIds :: Int -> String -> [Int]
nextCardIds id str = let
    numString = tail $ dropWhile (/= ':') str
    (winning, draw) = splitOut (words numString) "|"
    winningSet = fromList $ map read winning
    drawNumbers = map (read :: String -> Int) draw
    amount = foldl (\acc value -> if member value winningSet then acc + 1 else acc) 0 drawNumbers
    in take amount [id + 1, id + 2 ..]

type CardCountMap = Map Int Int
type Card = (Int, String)

expandCardsSet :: CardCountMap -> Card -> Int -> CardCountMap
expandCardsSet countMap card amount = let
    (id, str) = card
    nextIds = nextCardIds id str
    in foldl (\m i -> insert i (findWithDefault 0 i m + amount) m) countMap nextIds

day4b :: String -> Int
day4b content = let
    cards = lines content
    initialMap = Map.fromList $ take (length cards) [(id, 1) | id <- [1..]]
    resultMap = foldl (\m card -> expandCardsSet m card (findWithDefault 0 (fst card) m)) initialMap (zip [1..] cards)
    in sum $ elems resultMap
