module Day8 where
import Text.Regex.Posix
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Debug.Trace
import Data.List(isSuffixOf)
import Data.Map(Map, findWithDefault, fromListWith, toList, insert, (!))

newtype NodeDescription = NodeDescription (String, String, String) deriving (Show, Eq)
data Node = Node { left :: String, right :: String } deriving (Show, Eq)
data MapNode = MapNode { leftN :: Maybe MapNode , rightN :: Maybe MapNode } deriving (Show, Eq)

pattern = "([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)"

parseNodes :: [String] -> [NodeDescription]
parseNodes [] = []
parseNodes (l:ls) = let
    (_, _, _, [a, b, c]) = l =~ pattern :: (String, String, String, [String])
    in NodeDescription (a, b, c):parseNodes ls

buildMap :: [NodeDescription] -> Map String Node
buildMap = foldl (\m (NodeDescription (n, l, r)) -> Map.insert n (Node l r) m) Map.empty

parseMap :: [String] -> Map String Node
parseMap = buildMap . parseNodes

traverseMap :: Map String Node -> String -> String -> (String -> Bool) -> Int
traverseMap m current (cur:rest) predicate = let
    stop = predicate current
    next = case cur of
        'L' -> left $ m ! current
        'R' -> right $ m ! current
    in if stop then 0 else 1 + traverseMap m next rest predicate

day8a :: String -> Int
day8a content = let
    l = lines content
    path = head l
    pathMap = parseMap $ drop 2 l
    m = trace(show pathMap) pathMap
    in traverseMap pathMap "AAA" (cycle path) (=="ZZZ")

lcd :: Integral a => a -> a -> a
lcd a b = abs(a * b) `div ` gcd a b    

day8b :: String -> Int
day8b content = let
    l = lines content
    path = head l
    pathMap = parseMap $ drop 2 l
    m = trace(show pathMap) pathMap
    startNodes = filter (isSuffixOf "A") $ Map.keys pathMap
    nums = map (\node -> traverseMap pathMap node (cycle path) (isSuffixOf "Z")) startNodes
    in foldl lcd 1 nums

