import Data.List
import System.IO

step :: Int -> Int
step 0 = 6
step n = pred n

zeros :: [Int] -> Int
zeros = length . filter (==0)

fish :: [Int] -> [Int]
fish xs = map step xs ++ replicate (zeros xs) 8

main = do
    i <- readFile "input06.txt"
    let g0 = read ('[':i++"]") :: [Int]
    let g = g0 : map fish g :: [[Int]]
    putStrLn $ show $ length $ g !! 80
