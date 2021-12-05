import Data.List

main = interact solve

majority :: Int -> [String] -> Char
majority n xs = if
    (length $ filter (=='1') $ map (!!n) xs) * 2 >= length xs
    then '1' else '0'

minority :: Int -> [String] -> Char
minority n xs = if
    (length $ filter (=='0') $ map (!!n) xs) * 2 <= length xs
    then '0' else '1'

filt :: (Int -> [String] -> Char) -> Int -> [String] -> [String]
filt ority n xs = filter f xs where
    f x = x!!n == ority n xs

dig :: (Int -> [String] -> Char) -> Int -> [String] -> String
dig _ _ [x] = x
dig ority n xs = dig ority (succ n) $ filt ority n xs

fromBinary :: String -> Int
fromBinary = foldl go 0 where
  go n d = (n + n) + if d == '0' then 0 else 1

o2 :: [String] -> Int
o2 = fromBinary . dig majority 0

co2 :: [String] -> Int
co2 = fromBinary . dig minority 0

solveIt :: [String] -> Int
solveIt xs = o2 xs * co2 xs

solve :: String -> String
solve = (++"\n") . show . solveIt . lines
