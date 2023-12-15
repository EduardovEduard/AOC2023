module Main where

import Day15 (day15a, day15b)
import System.Environment (getArgs)

contentFromPath :: FilePath -> IO String
contentFromPath path = do
  readFile path

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
    run 15 [day15a, day15b]