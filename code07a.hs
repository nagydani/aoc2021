import Data.List

fuel :: Int -> [Int] -> Int
fuel d xs = sum $ map (\x -> abs(x - d)) xs

minFuel :: [Int] -> Int
minFuel xs =
  let rs = [minimum xs .. maximum xs]
      fs = map (\r -> fuel r xs) rs
  in minimum fs

main = do
  f <- readFile "input07.txt"
  let hs = read $ '[':f++"]" :: [Int]
  putStrLn $ show $ minFuel hs
