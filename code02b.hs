import Data.List

main = interact solve

readIt :: String -> [[String]]
readIt = map words . lines

navigate :: [[String]] -> (Int, Int, Int)
navigate = foldl nav (0, 0, 0) where
  nav (h, d, a) [xs, ns] = let n = read ns in case xs of
     "forward" -> (h + n, d + n*a, a)
     "down" -> (h, d, a + n)
     "up" -> (h, d, a - n)

multiply :: (Int, Int, Int) -> Int
multiply (x, y, _) = x * y

solve :: String -> String
solve = (++"\n") . show . multiply . navigate . readIt
