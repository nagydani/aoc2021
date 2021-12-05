import Data.List

main = interact solve

readIt :: String -> [[String]]
readIt = map words . lines

navigate :: [[String]] -> (Int, Int)
navigate = foldl nav (0, 0, 0) where
  nav (h, d) [xs, ns] = let n = read ns in case xs of
     "forward" -> (h + n, d)
     "down" -> (h, d + n)
     "up" -> (h, d - n)

multiply :: (Int, Int) -> Int
multiply (x, y) = x * y

solve :: String -> String
solve = (++"\n") . show . multiply . navigate . readIt
