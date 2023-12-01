module Main where
import Data.Char (isDigit, digitToInt)
import Data.Bits (Bits(xor))
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

main :: IO ()
main = do
  -- Parse the input file 
  lines <- linesFromPath "1.Calibration/input.txt"
  s <- (return . findCalibration) lines
  print s

linesFromPath :: FilePath -> IO [String]
linesFromPath path = do
  content <- readFile path
  return (lines content)

processLine :: Pattern -> String -> Int
processLine (Pattern p t) [] = 0
processLine (Pattern p t) line = case t((t line) =~ p :: String) of 
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

findCalibration :: [String] -> Int
findCalibration = foldr (\ x -> (+) (10 * (processLine straight x) + (processLine inverse x))) 0