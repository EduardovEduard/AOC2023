module Day2 where

import Data.Char (isDigit, digitToInt, isSpace)
import Data.List (isPrefixOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char (digit, char, string, space)
import Text.Parsec.Combinator (many1, sepBy)
import Control.Monad (void)
import Control.Applicative ((<|>), many, optional)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Foldable (fold)
import Text.Parsec (parse)
import Data.Either (rights)

data GameState = GameState { gid :: Int, gr :: Int, gg :: Int, gb :: Int } deriving (Show, Eq)
data TurnState = TurnState { tr :: Int, tg :: Int, tb :: Int } deriving (Show, Eq)

instance Ord GameState where
  compare (GameState _ r1 g1 b1) (GameState _ r2 g2 b2)
    | r1 == r2 && g1 == g2 && b1 == b2 = EQ
    | r1 > r2 || g1 > g2 || b1 > b2 = GT
    | otherwise = LT

baseState :: GameState
baseState = GameState 0 12 13 14

instance Semigroup TurnState where
  (TurnState r1 g1 b1) <> (TurnState r2 g2 b2) = TurnState (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid TurnState where
  mempty = TurnState 0 0 0

type GameParser = Parser GameState

numberParser :: Parser Int
numberParser = read <$> many1 digit

comma :: Parser ()
comma = do
    void $ char ','
    void $ optional (char ' ')

whitespace :: Parser ()
whitespace = void $ many1 space

colorParser :: Parser String
colorParser = string "blue" <|> string "red" <|> string "green"

semicolonParser :: Parser ()
semicolonParser = void $ string "; "

gameIDParser :: Parser Int
gameIDParser = do
    _ <- string "Game "
    number <- many1 digit
    _ <- char ':'
    return $ read number

colorNumberPairParser  :: Parser (String, Int)
colorNumberPairParser = do
    number <- numberParser
    _ <- whitespace
    color <- colorParser
    _ <- optional comma
    return (color, number)

sectionParser :: Parser TurnState
sectionParser = do
    pairs <- many colorNumberPairParser
    let colorMap = Map.fromList pairs
    let getOrDefault color = Map.findWithDefault 0 color colorMap
    return $ TurnState (getOrDefault "red") (getOrDefault "green") (getOrDefault "blue")


maxTurnStateParser :: Parser TurnState
maxTurnStateParser = do
    sections <- sectionParser `sepBy` semicolonParser
    return $ fold sections

gameParser :: Parser GameState
gameParser = do
    gameID <- gameIDParser
    _ <- whitespace
    turnState <- maxTurnStateParser
    return $ GameState gameID (tr turnState) (tg turnState) (tb turnState)

runGames :: [String] -> [GameState]
runGames = rights . fmap (parse gameParser "input")

day2a :: String -> Int
day2a content =
    let games = runGames $ lines content
        validGames = filter (<= baseState) games
    in sum $ map gid $ validGames

day2b :: String -> Int
day2b content =
    let games = runGames $ lines content
    in sum $ map (\game -> gr game * gg game * gb game) games

