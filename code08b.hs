import Data.List
import Data.Char

len :: Int -> [String] -> [String]
len n = filter (\x -> length x == n)

zero, one, two, three, four, five, six, seven, eight, nine :: [String] -> String
one = head . len 2
seven = head . len 3
four = head . len 4
eight = head . len 7
three xs = head $ filter (\x -> length (intersect x1 x) == 2) $ len 5 xs where
    x1 = one xs
two xs = head $ filter (\x -> length (intersect x4 x) == 2) $ len 5 xs where
    x4 = four xs
five xs = head $ filter (\x -> x /= x2 && x /= x3) $ len 5 xs where
    x2 = two xs
    x3 = three xs
six xs = head $ filter (\x -> length (intersect x1 x) == 1) $ len 6 xs where
    x1 = one xs
nine xs = head $ filter (\x -> length (intersect x3 x) == 5) $ len 6 xs where
    x3 = three xs
zero xs = head $ filter (\x -> x /= x6 && x /= x9) $ len 6 xs where
    x6 = six xs
    x9 = nine xs

dig :: [String] -> String -> Char
dig xs x = intToDigit $ sum $ lookup x $ zip 
                                          [zero xs, one xs, two xs, three xs, four xs,
                                           five xs, six xs, seven xs, eight xs, nine xs]
                                          [0..]

digs :: String -> Int
digs xs =
    let l = map sort $ words xs
        d = take 10 l
        e = drop 11 l
        ds = map (dig d) e
    in read ds

main = do
    f <- readFile "input08.txt"
    let ls = lines f
    let ns = map digs ls
    putStrLn $ show $ sum ns
