main = do
    f <- readFile "input08.txt"
    let i = length $ filter (\x -> x==2 || x==4 || x==3 || x==7) $ concatMap (map length) $ map words $ map (drop 61) (lines f)
    putStrLn $ show $ i 
