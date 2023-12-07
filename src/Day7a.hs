{-# LANGUAGE TupleSections #-}

module Day7a where

import qualified Data.Map as Map
import Data.Map(Map, findWithDefault, fromListWith, toList)
import Data.List(sort)

data HandType = High | Pair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Hand  = Hand { handType :: HandType, hand :: [Card], bid :: Int } deriving (Show, Eq, Ord)
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A deriving (Show, Eq, Ord, Enum)

countCards :: [Card] -> Map Int [Card]
countCards [] = Map.empty
countCards cards = let
    counter = fromListWith (+) . map (,1) $ cards
    in reverseMap counter
        where reverseMap :: Map Card Int -> Map Int [Card]
              reverseMap = fromListWith (++) . map (\(a,b) -> (b,[a])) . toList

charToCard :: Char -> Card
charToCard '2' = Two
charToCard '3' = Three
charToCard '4' = Four
charToCard '5' = Five
charToCard '6' = Six
charToCard '7' = Seven
charToCard '8' = Eight
charToCard '9' = Nine
charToCard 'T' = Ten
charToCard 'J' = J
charToCard 'Q' = Q
charToCard 'K' = K
charToCard 'A' = A

determineHandType :: [Card] -> HandType
determineHandType hand = let
    countMap = countCards hand
    five = length $ findWithDefault [] 5 countMap
    four = length $ findWithDefault [] 4 countMap
    three = length $ findWithDefault [] 3 countMap
    two = length $ findWithDefault [] 2 countMap
    one = length $ findWithDefault [] 1 countMap
    in case (five, four, three, two, one) of
        (1, _, _, _, _) -> FiveOfAKind
        (_, 1, _, _, _) -> FourOfAKind
        (_, _, 1, 1, _) -> FullHouse
        (_, _, 1, _, _) -> ThreeOfAKind
        (_, _, _, 2, _) -> TwoPairs
        (_, _, _, 1, _) -> Pair
        otherwise -> High

day7a :: String -> Int
day7a content = let
    hands = map (makeHand . words) $ lines content
    in foldl (\acc (hand, i) -> acc + (bid hand * i)) 0 $ zip (sort hands) [1..]
        where makeHand :: [String] -> Hand
              makeHand (hand:bid:_) = let
                cards = map charToCard hand
                in Hand (determineHandType cards) cards (read bid)


day7b :: String -> Int
day7b content = 0

