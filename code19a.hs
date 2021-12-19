import Data.List
import qualified Data.Set as S
import Control.Monad

shift :: Int -> [a] -> [a]
shift n xs = drop n xs ++ take n xs

cross :: [Int] -> [Int] -> [Int]
[a, b, c] `cross` [d, e, f] =
    [b*f - c*e, c*d - a*f, a*e - b*d]

add, sub :: [Int] -> [Int] -> [Int]
add xs = zipWith (+) xs
sub xs = zipWith (-) xs

scale :: Int -> [Int] -> [Int]
scale n = map (*n)

rot :: [[Int]] -> [Int] -> [Int]
rot m a = foldl1' add $ zipWith scale a m

orient :: [[[Int]]]
orient = do
    c <- [1, -1]
    s <- [0 .. 2]
    let x = shift s [c, 0, 0]
    d <- [1, -1]
    u <- [0, 1]
    let y = shift s $ 0 : shift u [d, 0]
    let z = x `cross` y
    return [x, y, z]
