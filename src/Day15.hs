module Day15 where
import Data.Char(ord, digitToInt)
import qualified Data.Text as T
import Data.Text(splitOn)
import Debug.Trace(trace)
import Data.List(isSuffixOf)
import qualified Data.Map as Map
import Data.Map(Map, findWithDefault, fromListWith, toList, insert, (!), adjust)

data Lens = Lens String Int deriving (Show, Eq)
newtype Box = Box [Lens] deriving (Show, Eq)
data Op = Add String Int | Remove String deriving (Show, Eq)

hash :: String -> Int
hash = foldl (\acc c -> (17 * (acc + ord c)) `mod` 256 ) 0

day15a :: String -> Int
day15a content = let
    words = splitOn (T.pack ",") (T.pack content)
    in sum $ map (hash . T.unpack) words


parseOp :: String -> Op
parseOp s = if "-" `isSuffixOf` s then Remove (init s) else Add (takeWhile (/='=') s) (digitToInt $ last s)

removeLens :: [Lens] -> String -> [Lens]
removeLens [] _ = []
removeLens (l@(Lens label _) : ls) s = if label == s then ls else l:removeLens ls s

updateBox :: [Lens] -> Lens -> [Lens]
updateBox [] lens = [lens]
updateBox (l@(Lens label f) : ls) newl@(Lens newlabel newf) = if label == newlabel then Lens label newf:ls else l:updateBox ls newl

applyCommands :: [Op] -> Map Int Box
applyCommands = foldl (flip applyCommand) (foldl (\m i -> Map.insert i (Box []) m) Map.empty [0..255])
    where applyCommand :: Op -> Map Int Box -> Map Int Box
          applyCommand (Add s i) m = let
            update (Box lenses) = Box $ updateBox lenses (Lens s i)
            in Map.adjust update (hash s) m
          applyCommand (Remove s) m = let
            update (Box lenses) = Box $ removeLens lenses s
            in Map.adjust update (hash s) m

focusPower :: Map Int Box -> Int
focusPower = Map.foldrWithKey (\i (Box lenses) acc -> acc + (i + 1) * sum (zipWith (curry (\(i, Lens _ f) -> i * f)) [1..] lenses)) 0

day15b :: String -> Int
day15b content = let
    words = splitOn (T.pack ",") (T.pack content)
    commands = map (parseOp . T.unpack) words
    boxes = applyCommands commands
    b = trace (show boxes) boxes
    in focusPower b
