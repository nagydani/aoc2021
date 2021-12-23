import Data.List
import qualified Data.Set as S
import Control.Monad

type Point = (Int, Int)

start :: [[Point]]
start = [[(2,2),(2,8)],[(1,2),(1,6)],[(1,4),(2,6)],[(1,8),(2,4)]]

occupied :: [[Point]] -> Point -> Bool
occupied ps p = not $ null $ elemIndices p $ concat ps

blocked :: [[Point]] -> [Int] -> Bool
blocked ps is = or $ map (\x -> occupied ps (0, x)) is
    
manhattan :: Point -> Point -> Int
manhattan (y1, x1) (y2, x2) =
    abs (y2 - y1) + abs (x2 - x1)
    
moves :: [[Point]] -> [([[Point]], Int)]
moves ps = do
    label <- [0..3]
    num <- [0..1]
    let (y, x) = (ps !! label) !! num
    let dest = 2 + 2 * label
    -- not at destination
    guard $ not $ y > 0 && x == dest && (y == 2 || (ps !! label) !! 1 == (2, dest))
    -- not buried
    guard $ not $ y == 2 && occupied ps (1, x)
    -- destination not blocked
    guard $ not $ y == 0 && occupied ps (1, dest)
    -- destination not occupied by alien
    guard $ not $ y == 0 && occupied ps (2, dest) && (ps !! label) !! 1 /= (2, dest)
    -- hallway to destination not blocked
    guard $ not $ y == 0 && blocked ps ([x+1..dest-1] ++ [dest+1..x-1])
    -- from hallway to destination
    let targets = if y == 0
        then [(if occupied ps (2, dest) then 1 else 2, dest)]
        else [(0,0), (0,1), (0,3), (0,5), (0,7), (0,9), (0,10)]
    (y', x') <- targets
    -- hallway to target not blocked
    guard $ not $ y > 0 && blocked ps ([x'..x] ++ [x..x'])
    --
    let cost = (manhattan (y, x) (y', x')) * 10^label
    let final = (take label ps) ++
                sort [(y', x'), (ps !! label) !! (1 - num)] :
                (drop (succ label) ps)
    return (final, cost)
