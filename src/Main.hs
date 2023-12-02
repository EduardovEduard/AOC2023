module Main where

import Day2 (day2a, day2b)
import System.Environment (getArgs)

contentFromPath :: FilePath -> IO String
contentFromPath path = do
  content <- readFile path
  return content

getSuffix :: IO String
getSuffix = do
    args <- getArgs
    pure $ if null args then "" else "_" ++ head args

getDayInput :: Int -> IO String
getDayInput day = do
    suffix <- getSuffix
    contentFromPath $ "input/day" ++ show day ++ "/input" ++ suffix ++ ".txt"

run :: Int -> [String -> Int] -> IO()
run day funcs = do
    input <- getDayInput day
    mapM_ (print . ($ input)) funcs

main :: IO()
main = do
    run 2 [day2a, day2b]