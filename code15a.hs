import Data.Char
import Data.List
import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad

type Point = (Int, Int)
data Distance = D Int | Infty deriving (Eq, Ord)

dist :: Distance -> Int
dist (D x) = x

dijkstra :: Array Point Int -> Int
dijkstra a =
    let unvisited = S.fromList $ indices a
        distances = M.insert (0, 0) (D 0) $ M.fromSet (\_ -> Infty) unvisited
        current = (0, 0)
    in dijkstra3 a unvisited distances current

addDist :: Array Point Int -> Point -> 
                M.Map Point Distance -> Point -> M.Map Point Distance
addDist a current distances p =
    M.insert p (minimum [distances M.! p,
                         D (dist (distances M.! current) + a ! p)]) distances

compareDist :: M.Map Point Distance -> Point -> Point -> Ordering
compareDist d a b = compare (d M.! a) (d M.! b)

smallestDist :: M.Map Point Distance -> S.Set Point -> Point
smallestDist distances unvisited =
    minimumBy (compareDist distances) (S.elems unvisited)

dijkstra3 :: Array Point Int ->
    S.Set Point -> M.Map Point Distance -> Point -> Int
dijkstra3 a unvisited distances current =
    let ((_, _), (bottom, right)) = bounds a
        (y, x) = current
        neighbors = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        unvisitedNeighbors = filter (\x -> S.member x unvisited) neighbors
        distances' = foldl' (addDist a current) distances unvisitedNeighbors
        unvisited' = S.delete current unvisited
    in if current == (bottom, right) then dist $ distances' M.! current
        else dijkstra3 a unvisited' distances' $
                       smallestDist distances' unvisited'

main = do
    f <- readFile "input15.txt"
    let l = map (map digitToInt) $ lines f
    let h = length l
    let w = length $ head l
    let a = listArray ((0, 0), (pred h, pred w)) $ concat l
    putStrLn $ show $ dijkstra a
