import Data.List
import Data.Char
import qualified Data.Map.Strict as M 
import Text.Parsec
import Text.Parsec.String (Parser)

type Edge = (String, String)
type Map = M.Map String [String]
type Path = [String]

m ! k = M.findWithDefault [] k m

start = ["start", "start"] :: Path

small :: String -> Bool
small (x:xs) = isLower x

validPath :: Path -> Bool
validPath p =
    let ns = map length $ group $ sort $ filter small p
        g2 = length $ filter (>2) ns
        e2 = length $ filter (==2) ns
    in g2 == 0 && e2 <= 2

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

addCave :: Path -> String -> Path
addCave xs c = c : xs

finished :: Path -> Bool
finished (x:xs) = x == "end"

path :: Map -> Path -> [Path]
path m p
    | finished p = [p]
    | otherwise = filter validPath $ map (addCave p) $ m ! (head p)

paths :: Map -> [Path] -> [Path]
paths m ps
    | and $ map finished ps = ps
    | otherwise = paths m $ ps >>= path m

main = do
    f <- readFile "input12.txt"
    let m = foldl' addEdge M.empty $ readIt f
    putStrLn $ show $ length $ paths m [start]
