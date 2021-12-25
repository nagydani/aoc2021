import Data.List
import Data.Array

type Game = (Int, Int, Int, Int)

countWin' :: Array Game (Integer, Integer)
countWin' = listArray ((0, 0, 0, 0), (9, 9, 40, 40)) $
    map countWin $ range ((0, 0, 0, 0), (9, 9, 40, 40))

countWin :: Game -> (Integer, Integer)
countWin (p1, p2, s1, s2)
    | s1 >= 21 = (1, 0)
    | s2 >= 21 = (0, 1)
    | otherwise =
        let ds = [ d1 + d2 + d3 | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3] ]
        in foldl' go (0, 0) ds where
            go (x, y) d =
                let p1' = (p1 + d) `mod` 10
                    s1' = s1 + p1' + 1
                    (x', y') = countWin' ! (p2, p1', s2, s1')
                in (x + y', y + x')

main = do
    let (x, y) = countWin (6 - 1, 2 - 1, 0, 0)
    putStrLn $ show $ maximum [x, y]
