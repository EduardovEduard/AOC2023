module Day1 where

import Data.Char (isDigit, digitToInt)
import Text.Regex.Posix ((=~))


straightPattern :: String
straightPattern = "one|two|three|four|five|six|seven|eight|nine|[0-9]"

inversePattern :: String
inversePattern = "eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9]"

data Pattern = Pattern String (String -> String)

straight :: Pattern
straight = Pattern straightPattern id

inverse :: Pattern
inverse = Pattern inversePattern reverse


processLineA :: String -> Int
processLineA (x:xs)
  | isDigit x = digitToInt x
  | otherwise = processLineA xs
processLineA [] = 0

processLineB :: Pattern -> String -> Int
processLineB (Pattern p t) [] = 0
processLineB (Pattern p t) line = case t((t line) =~ p :: String) of 
  "" -> 0
  "one" -> 1
  "two" -> 2
  "three" -> 3
  "four" -> 4
  "five" -> 5
  "six" -> 6
  "seven" -> 7
  "eight" -> 8
  "nine" -> 9
  x -> read x 

findCalibrationA :: [String] -> Int
findCalibrationA = foldr (\ x -> (+) (10 * processLineA x + processLineA (reverse x))) 0

day1a :: String -> Int
day1a content = findCalibrationA (lines content)

findCalibrationB :: [String] -> Int
findCalibrationB = foldr (\ x -> (+) (10 * (processLineB straight x) + (processLineB inverse x))) 0

day1b :: String -> Int
day1b content = findCalibrationB (lines content)
