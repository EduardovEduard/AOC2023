module Main where
import Data.Char (isDigit, digitToInt)
import Data.Bits (Bits(xor))

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

processLine :: String -> Int
processLine (x:xs)
  | isDigit x = digitToInt x
  | otherwise = processLine xs
processLine [] = 0

findCalibration :: [String] -> Int
findCalibration = foldr (\ x -> (+) (10 * processLine x + processLine (reverse x))) 0