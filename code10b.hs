import Data.List

type Stack = ([Char],Int)

start = (".",0) :: Stack

check :: Stack -> Char -> Stack
check (x:xs, s) c = case c of
    '(' -> (')':x:xs, s)
    '[' -> (']':x:xs, s)
    '{' -> ('}':x:xs, s)
    '<' -> ('>':x:xs, s)
    ')' -> if x == c then (xs, s) else (xs, s + 3)
    ']' -> if x == c then (xs, s) else (xs, s + 57)
    '}' -> if x == c then (xs, s) else (xs, s + 1197)
    '>' -> if x == c then (xs, s) else (xs, s + 25137)

err1 :: String -> Int
err1 xs = 
    let e = dropWhile (\(_,n) -> n == 0) $ scanl' check start xs
    in if null e then 0 else snd $ head e

score :: Int -> Char -> Int
score n c = n * 5 + case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4

auto :: String -> Int
auto xs =
    let e = init $ fst $ foldl' check start xs
    in foldl' score 0 e

main = do
    f <- readFile "input10.txt"
    let l = filter (\xs -> 0 == err1 xs) $ lines f
    let s = sort $ map auto l
    putStrLn $ show $ s !! ((length s) `div` 2)
