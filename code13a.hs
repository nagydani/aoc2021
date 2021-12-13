import qualified Data.Set as S
import Data.List
import Text.Parsec
import Text.Parsec
import Text.Parsec.String (Parser)

type Point = (Int, Int)
type Edge = (Bool, Int)
type Page = S.Set Point

point :: Parser Point
point = do
    x <- many digit
    char ','
    y <- many digit
    char '\n'
    return (read x, read y)

edge :: Parser Edge
edge = do
    string "fold along "
    a <- letter
    char '='
    v <- many digit
    char '\n'
    return (a == 'x', read v)

input :: Parser ([Point], [Edge])
input = do
    ps <- many point
    char '\n'
    es <- many edge
    return (ps, es)

readIt :: Either a ([Point], [Edge]) -> (Page, [Edge])
readIt (Left _) = (S.empty, [])
readIt (Right (ps, es)) = (foldr S.insert S.empty ps, es)

foldUp :: Edge -> Point -> Point
foldUp (a, v) (x, y)
    | a         = (if x > v then v + v - x else x, y)
    | otherwise = (x, if y > v then v + v - y else y)

foldOnce :: Page -> Edge -> Page
foldOnce ps e = S.map (foldUp e) ps

main = do
    f <- readFile "input13.txt"
    let (ps, es) = readIt $ parse input "" f
    putStrLn $ show $ S.size $ foldOnce ps $ head es
