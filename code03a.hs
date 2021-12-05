import Data.List

main = interact solve

countIt :: [String] -> [Int]
countIt (x:xs) =
    let start '0' = (-1)
        start '1' = 1
        s = map start x
        u n '0' = n - 1
        u n '1' = n + 1
        go ns ys = zipWith u ns ys
    in  foldl go s xs

fromBin :: [Int] -> Int
fromBin = foldl go 0 where
    go n d = (n + n) + if d > 0 then 1 else 0

solveIt :: [Int] -> Int
solveIt ns = (gamma ns) * (epsilon ns) where
    gamma = fromBin
    epsilon = fromBin . map negate

solve :: String -> String
solve = (++"\n") . show . solveIt . countIt . lines
