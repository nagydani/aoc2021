import Data.List
import System.IO

type Point = (Int, Int)
type Line = (Point, Point)

pair :: String -> Point
pair xs = read ('(':xs++")")

dots :: Line -> [Point]
dots ((x1,y1),(x2,y2))
  | x1 == x2 && y1 < y2 = map (\y -> (x1,y)) [y1..y2]
  | x1 == x2 && y1 > y2 = map (\y -> (x1,y)) [y2..y1]
  | y1 == y2 && x1 < x2 = map (\x -> (x,y1)) [x1..x2]
  | y1 == y2 && x1 > x2 = map (\x -> (x,y1)) [x2..x1]
  | otherwise = []

vents :: [Line] -> [Point]
vents = sort . concatMap dots

more :: Eq a => [a] -> Int
more = length . filter (>1) . map length . group

main = do
    f <- readFile "input05.txt"
    let ls = map (\xs -> (pair(xs !! 0), pair(xs !! 2))) $ map words $ lines f
    let vs = vents ls
    putStrLn $ show $ more vs
