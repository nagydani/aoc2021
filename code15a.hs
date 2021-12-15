import Data.Char
import Data.List
import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad

type Point = (Int, Int)
data Distance = D Int | Infty deriving (Eq, Ord)
type DistanceMap = M.Map Point Int

at :: DistanceMap -> Point -> Distance
at m p = if M.member p m then D (m M.! p) else Infty

dist :: Distance -> Int
dist (D x) = x

dijkstra :: Array Point Int -> Int
dijkstra a =
    let unvisited = S.fromList $ indices a
        distances = M.singleton (0, 0) 0
        current = (0, 0)
    in dijkstra3 a unvisited distances current

addDist :: Array Point Int -> Point -> 
                DistanceMap -> Point -> DistanceMap
addDist a current distances p =
    M.insert p (dist (minimum [distances `at` p,
                         D (dist (distances `at` current) + a ! p)])) distances

compareDist :: DistanceMap -> Point -> Point -> Ordering
compareDist d a b = compare (d `at` a) (d `at` b)

smallestDist :: DistanceMap -> Point
smallestDist distances =
    minimumBy (compareDist distances) (M.keys distances)

dijkstra3 :: Array Point Int ->
    S.Set Point -> DistanceMap -> Point -> Int
dijkstra3 a unvisited distances current =
    let ((_, _), (bottom, right)) = bounds a
        (y, x) = current
        neighbors = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        unvisitedNeighbors = filter (\x -> S.member x unvisited) neighbors
        distances' = foldl' (addDist a current) distances unvisitedNeighbors
        unvisited' = S.delete current unvisited
        distances'' = M.delete current distances'
    in if current == (bottom, right) then dist $ distances' `at` current
        else dijkstra3 a unvisited' distances'' $ smallestDist distances''

main = do
    f <- readFile "input15.txt"
    let l = map (map digitToInt) $ lines f
    let h = length l
    let w = length $ head l
    let a = listArray ((0, 0), (pred h, pred w)) $ concat l
    putStrLn $ show $ dijkstra a
