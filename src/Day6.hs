module Day6 where

data Race =  Race { time :: Double, recordS :: Double } deriving (Show)

getRaces :: String -> ([String] -> [String]) -> [Race]
getRaces content transform = let 
    l = lines content
    times = map read $ transform $ tail $ words (l !! 0)
    distances = map read $ transform $ tail $ words (l !! 1)
    in map (uncurry Race) $ zip times distances

solveRace :: Race -> Int
solveRace (Race t s) = let 
    d = t ^ 2 - 4 * s
    t1 = (t + sqrt d) / 2
    t2 = (t - sqrt d) / 2
    in ceiling t1 - floor t2 - 1

day6a :: String -> Int
day6a content = product $ map solveRace $ getRaces content id

day6b :: String -> Int
day6b content = product $ map solveRace $ getRaces content (pure . concat)