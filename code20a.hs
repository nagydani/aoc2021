import qualified Data.Set as S
import Data.Array
import Data.List

type Point = (Int, Int)
type Grid = S.Set Point
type Algo = Array (Int) Bool

readInput :: [[Char]] -> Grid
readInput ls =
    let h = length ls
        w = length $ head ls
        ixs = [ (y, x) | y <- [1..h], x <- [1..w] ]
    in S.fromList $ map fst $ filter (\x -> snd x == '#') $ zip ixs $ concat ls

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\a d -> a + a + boolToInt d) 0
    where boolToInt b = if b then 1 else 0

neighbors :: Point -> [Point]
neighbors (y, x) =
     [ (a, b) | a <- [pred y .. succ y], b <- [pred x .. succ x] ]

hood :: Grid -> Point -> Int
hood s (y, x) =
    let ps = neighbors (y, x)
        bs = map ((flip S.member) s) ps
    in fromBinary bs 

update :: Algo -> Grid -> Point -> (Grid -> Grid)
update a g p = if a ! (hood g p)
    then S.insert p
    else S.delete p

scope :: Grid -> Grid
scope = S.fromList . concatMap neighbors . S.toList

step :: Algo -> Grid -> Grid
step a g =
    let s = S.toList $ scope g :: [Point]
        us = map (update a g) s :: [Grid -> Grid]
    in (foldl1' (.) us) g

main = do
    f <- readFile "input20.txt"
    let (l0:l1:ls) = lines f
    let algo = listArray (0,511) $ map (== '#') l0
    let s0 = readInput ls
    putStrLn $ show $ S.size $ step algo $ step algo s0
