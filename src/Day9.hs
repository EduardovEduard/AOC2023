module Day9 where

line :: String -> [Int]
line = map read . words 

extrapolate :: [Int] -> Int
extrapolate [] = 0
extrapolate (x:xs) = extrapolate' x xs []
    where extrapolate' :: Int -> [Int] -> [Int] -> Int
          extrapolate' last [] diffs = last + extrapolate diffs
          extrapolate' cur (a:as) diffs = extrapolate' a as (diffs ++ [a - cur])

day9a :: String -> Int
day9a content = let 
    numLines = map line $ lines content
    in sum $ map extrapolate numLines

day9b :: String -> Int
day9b content = let 
    numLines = map line $ lines content
    in sum $ map (extrapolate . reverse) numLines