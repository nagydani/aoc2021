import qualified Data.Set as S
import Data.Array
import Data.List

type Point = (Int, Int)
type Grid = (Bool, S.Set Point)
type Algo = Array (Int) Bool

readInput :: [[Char]] -> Grid
readInput ls =
    let h = length ls
        w = length $ head ls
        ixs = [ (y, x) | y <- [1..h], x <- [1..w] ]
    in (False, S.fromList $ map fst $ filter (\x -> snd x == '#') $ zip ixs $ concat ls)

lit :: Grid -> Point -> Bool
lit (i, g) p = if i then not $ S.member p g else S.member p g

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\a d -> a + a + boolToInt d) 0
    where boolToInt b = if b then 1 else 0

neighbors :: Point -> [Point]
neighbors (y, x) =
     [ (a, b) | a <- [pred y .. succ y], b <- [pred x .. succ x] ]

hood :: Grid -> Point -> Int
hood g (y, x) =
    let ps = neighbors (y, x)
        bs = map (lit g) ps
    in fromBinary bs 

update :: Algo -> Grid -> Point -> (S.Set Point -> S.Set Point)
update a g p =
    let i = fst g
    in if a ! (hood g p)
        then if i then S.insert p else id
        else if i then id else S.insert p

scope :: S.Set Point -> S.Set Point
scope = S.fromList . concatMap neighbors . S.toList

step :: Algo -> Grid -> Grid
step a (i, g) =
    let s = S.toList $ scope g :: [Point]
        us = map (update a (i, g)) s :: [S.Set Point -> S.Set Point]
    in if a ! 0
        then (not i, (foldl1' (.) us) S.empty)
        else (i, (foldl1' (.) us) S.empty)

main = do
    f <- readFile "input20.txt"
    let (l0:l1:ls) = lines f
    let algo = listArray (0,511) $ map (== '#') l0
    let s0 = readInput ls
    let s = s0 : map (step algo) s
    putStrLn $ show $ S.size $ snd $ (s !! 2)
