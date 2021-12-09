import Data.List
import Data.Char

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

risk :: [[Char]] -> Int
risk xs =
    let ls = concat $ lows xs
        hs = concat xs
        rs = zipWith (\l h -> if l then succ (digitToInt h) else 0) ls hs
    in sum rs

main = do
    f <- readFile "input09.txt"
    let l = lines f
    putStrLn $ show $ risk l
