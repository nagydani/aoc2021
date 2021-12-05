import System.IO
import Data.List

type Row = [Int]
type Board = [Row]

readNums :: Handle -> IO [Int]
readNums h = do
    l <- hGetLine h
    return (read ('[':l++"]"))

readBoards :: String -> [Board]
readBoards f =
  let ls = lines f
      ws = map words ls
      ns = map (map read) ws
      bs = map tail $ groupBy (\_ x -> x /= []) ns
  in map (\xs -> xs ++ transpose xs) bs

rowWins :: [Int] -> Row -> Bool
rowWins ns r = and $ map (`elem` ns) r

wins :: [Int] -> Board -> Bool
wins ns b = or $ map (rowWins ns) b

-- unsafe head
winningInit :: [Int] -> Board -> [Int]
winningInit ns b = head $ filter (\i -> wins i b) $ inits ns

win :: [Int] -> [Board] -> [Board]
win ns bs = filter (wins ns) bs

winFirst :: [Int] -> [Board] -> [Board]
winFirst ns bs = head $ dropWhile null $ map (\xs -> win xs bs) $ inits ns   

main = do
    fh <- openFile "input04.txt" ReadMode
    nums <- readNums fh
    rest <- hGetContents fh
    let boards = readBoards rest
    let winner = head $ winFirst nums boards
    let wnums = winningInit nums winner
    let unmarked = filter (`notElem` wnums) $ concat winner
    putStrLn $ show $ (sum unmarked `div` 2) * (last wnums)
