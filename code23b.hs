import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad

type Point = (Int, Int)
type Cave = [[Point]]
type DistanceMap = (M.Map Cave Int, S.Set (Int, Cave))
data Distance = D Int | Infty deriving (Eq, Ord)

start :: Cave
start = [[(1,4),(2,8),(3,6),(4,8)]  -- A
        ,[(1,2),(1,6),(2,6),(3,4)]  -- B
        ,[(1,8),(2,4),(3,8),(4,2)]  -- C
        ,[(2,2),(3,2),(4,4),(4,6)]] -- D

final :: Cave
final = [[(1,2),(2,2),(3,2),(4,2)]
        ,[(1,4),(2,4),(3,4),(4,4)]
        ,[(1,6),(2,6),(3,6),(4,6)]
        ,[(1,8),(2,8),(3,8),(4,8)]]

occupied :: Cave -> Point -> Bool
occupied ps p = not $ null $ elemIndices p $ concat ps

blocked :: Cave -> [Int] -> Bool
blocked ps is = or $ map (\x -> occupied ps (0, x)) is

buried :: Cave -> Point -> Bool
buried ps (y, x) =
    let xs = [ (z, x) | z <- [1..pred y] ]
    in or $ map (\p -> occupied ps p) xs

atDest :: Cave -> Int -> Point -> Bool
atDest ps label (y, x) =
    let dest = 2 + 2 * label
        sis = ps !! label
        rs = [ (z, x) | z <- [succ y .. 4] ]
    in y > 0 && x == dest &&
       (and $ map (\p -> not $ null $ elemIndices p sis) rs )

alien :: Cave -> Int -> Point -> Bool
alien ps label (y, x) =
    let dest = 2 + 2 * label
        sis = ps !! label
        rs = filter (occupied ps) [ (z, dest) | z <- [2..4] ]
    in y == 0 &&
       (or $ map (\p -> null $ elemIndices p sis) rs )

manhattan :: Point -> Point -> Int
manhattan (y1, x1) (y2, x2) =
    abs (y2 - y1) + abs (x2 - x1)

depth :: Cave -> Int -> Int
depth ps c =
    let os = [ (y, c) | y <- [1..4], occupied ps (y, c) ]
    in if null os 
        then 4
        else let (y, _) = head os in pred y

moves :: Cave -> [(Cave, Int)]
moves ps = do
    label <- [0..3]
    num <- [0..3]
    let (y, x) = (ps !! label) !! num
    let dest = 2 + 2 * label
    -- not at destination
    guard $ not $ atDest ps label (y, x)
    -- not buried
    guard $ not $ y > 1 && buried ps (y, x)
    -- destination not blocked
    guard $ not $ y == 0 && occupied ps (1, dest)
    -- destination not occupied by alien
    guard $ not $ alien ps label (y, x)
    -- hallway to destination not blocked
    guard $ not $ y == 0 && blocked ps ([x+1..dest-1] ++ [dest+1..x-1])
    -- from hallway to destination
    let targets = if y == 0
        then [(depth ps dest, dest)]
        else [(0,0), (0,1), (0,3), (0,5), (0,7), (0,9), (0,10)]
    (y', x') <- targets
    -- hallway to target not blocked
    guard $ not $ y > 0 && blocked ps ([x'..x] ++ [x..x'])
    --
    let cost = (manhattan (y, x) (y', x')) * 10^label
    let final = (take label ps) ++
                sort ((y', x') : [(ps !! label) !! n | n <- [0..3], n /= num ]) :
                (drop (succ label) ps)
    return (final, cost)

(!) :: DistanceMap -> Cave -> Distance
(m, _) ! p = if M.member p m then D (m M.! p) else Infty

dist :: Distance -> Int
dist (D x) = x

del :: Cave -> DistanceMap -> DistanceMap
del p (distances, ordering) =
    if M.notMember p distances
        then (distances, ordering)
        else (M.delete p distances, S.delete (distances M.! p, p) ordering)

addDist :: Cave -> DistanceMap -> (Cave, Int) -> DistanceMap
addDist current (distances, ordering) (p, d) =
    let tent = (distances, ordering) ! p
        tent' = D ((distances M.! current) + d)
        m = dist $ minimum [tent, tent']
    in if M.member p distances
        then (M.insert p m distances, S.insert (m, p) $ S.delete (distances M.! p, p) ordering)
        else (M.insert p m distances, S.insert (m, p) ordering)

smallestDist :: DistanceMap -> Cave
smallestDist (_, ordering) = snd $ S.findMin ordering

dijkstra :: S.Set Cave -> DistanceMap -> Cave -> Int
dijkstra visited distances current =
    let unvisitedNeighbors = filter (\(x,_) -> S.notMember x visited) $
                             moves current
        distances' = foldl' (addDist current) distances unvisitedNeighbors
        visited' = S.insert current visited
        distances'' = del current distances'
    in if current == final
        then dist $ distances' ! current
        else dijkstra visited' distances'' $ smallestDist distances''

main = do
    putStrLn $ show $ dijkstra S.empty (M.singleton start 0, S.singleton (0, start) ) start
