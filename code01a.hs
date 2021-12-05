import Data.List

main = interact solve

toReadable :: String -> String
toReadable xs = '[' : (concat $ intersperse "," $ lines xs) ++ "]"

readIt :: String -> [Int]
readIt = read

countIncr :: Ord a => [a] -> Int
countIncr xs = length $ filter (==LT) $ zipWith compare xs (tail xs)

solve :: String -> String
solve = (++"\n") . show . countIncr . readIt . toReadable
