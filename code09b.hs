import Data.List
import Data.Char
import Data.Array

cmpL, cmpR, cmpU, cmpD :: [[Char]] -> [[Bool]]

cmpL rs = zipWith cmpLr rs rs where
    cmpLr xs ys = (zipWith (<) xs (tail ys)) ++ [True]

cmpR rs = zipWith cmpRr rs rs where
    cmpRr xs ys = True : (zipWith (<) (tail xs) ys)

cmpU xs = (zipWith (zipWith (<)) xs (tail xs)) ++ [replicate l True] where
    l = length $ head xs

cmpD xs = replicate l True : (zipWith (zipWith (<)) (tail xs) xs) where
    l = length $ head xs

zipAnd :: [[Bool]] -> [[Bool]] -> [[Bool]]
zipAnd = zipWith z where z = zipWith (&&)

lows :: [[Char]] -> [[Bool]]
lows xs = (cmpL xs) `zipAnd` (cmpR xs) `zipAnd` (cmpU xs) `zipAnd` (cmpD xs)

type Point = (Int,Int)

dims :: [[a]] -> Point
dims xs = (length xs,length $ head xs)

lowIxs :: [[Char]] -> [Point]
lowIxs xs =
    let d = dims xs
        a = filter snd $ assocs $ listArray ((1,1),d) (concat $ lows xs)
    in map fst a

ridges :: [[Char]] -> Array Point Bool
ridges xs = listArray ((1,1),dims xs) (map (== '9') $ concat xs) 

lo, hi :: Array Point a -> Point
lo arr = fst $ bounds arr
hi arr = snd $ bounds arr

outOfBounds :: Array Point a -> Point -> Bool
outOfBounds a (x,y)
    | x < fst (lo a) || x > fst (hi a) = True
    | y < snd (lo a) || y > snd (hi a) = True
    | otherwise = False

floodFill :: Point -> Array Point Bool -> Array Point Bool
floodFill p a
    | outOfBounds a p = a
    | a ! p = a
    | otherwise =
        floodFill (x+1,y) .
        floodFill (x-1,y) .
        floodFill (x,y+1) $
        floodFill (x,y-1) a'
    where (x,y) = p
          a' = a // [(p,True)]

tally :: Array Point Bool -> Int
tally = length . filter id . elems

basin :: Array Point Bool -> Point -> Int
basin a p =
    let f = floodFill p a
    in tally f - tally a

basins :: [[Char]] -> [Int]
basins xs =
    let b = lowIxs xs
        r = ridges xs
    in map (basin r) b

main = do
    f <- readFile "input09.txt"
    let l = lines f
    putStrLn $ show $ product $ take 3 $ sortOn negate $ basins l
