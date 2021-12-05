import Data.List

main = interact solve

toReadable :: String -> String
toReadable xs = '[' : (concat $ intersperse "," $ lines xs) ++ "]"

readIt :: String -> [Int]
readIt = read

window :: [Int] -> [Int]
window xs = zipWith (+) xs $ zipWith (+) (tail xs) (tail $ tail xs)

countIncr :: Ord a => [a] -> Int
countIncr xs = length $ filter (==LT) $ zipWith compare xs (tail xs)

solve :: String -> String
solve = (++"\n") . show . countIncr . window . readIt . toReadable
