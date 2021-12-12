import Data.List
import Data.Char
import qualified Data.Map.Strict as M 
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

type Edge = (String, String)
type Map = M.Map String [String]
type Visited = S.Set String
type Path = (Visited, [String])

m ! k = M.findWithDefault [] k m

start = (S.singleton "start", ["start"]) :: Path

cave :: Parser String
cave = many letter

edge :: Parser (String, String)
edge = do
    a <- cave
    char '-'
    b <- cave
    char '\n'
    return (a, b)

edges :: Parser [Edge]
edges = many edge

readIt :: String -> [Edge]
readIt xs = r $ parse edges "" xs where
    r (Left _) = []
    r (Right ys) = ys

addEdge :: Map -> Edge -> Map
addEdge m (x, y) = M.insert y (x : (m ! y)) $ M.insert x (y : (m ! x)) m

small :: String -> Bool
small (x:xs) = isLower x

visit :: Visited -> String -> Visited
visit v c
    | small c = S.insert c v
    | otherwise = v

addCave :: Path -> String -> Path
addCave (v, xs) c = (visit v c, c : xs)

finished :: Path -> Bool
finished (v, x:xs) = x == "end"

path :: Map -> Path -> [Path]
path m (v, x:xs)
    | x == "end" = [(v, x:xs)]
    | otherwise =
        let nextCaves = filter (`S.notMember` v) $ m ! x
        in map (addCave (v, x:xs)) nextCaves

paths :: Map -> [Path] -> [Path]
paths m ps
    | and $ map finished ps = ps
    | otherwise = paths m $ ps >>= path m

main = do
    f <- readFile "input12.txt"
    let m = foldl' addEdge M.empty $ readIt f
    putStrLn $ show $ length $ paths m [start]
