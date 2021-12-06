import Data.List
import System.IO

readIt :: [Integer] -> Int -> [Integer]
readIt xs n = take n xs ++ (succ $ xs !! n) : drop (succ n) xs

fish :: [Integer] -> [Integer]
fish (x:xs) = take 6 xs ++ ((x + xs!!6) : drop 7 xs) ++ [x]

main = do
    i <- readFile "input06.txt"
    let g0l = read ('[':i++"]") :: [Int]
    let g0 = foldl readIt (replicate 9 0) g0l :: [Integer]
    let g = g0 : map fish g
    putStrLn $ show $ sum $ g !! 256
