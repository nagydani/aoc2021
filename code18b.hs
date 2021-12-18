import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe
import Data.List
import Control.Monad

data SF = RN Int | Pair SF SF deriving (Show, Eq)

rn :: Parser SF
rn = do
    d <- many1 digit
    return $ RN (read d)

pair :: Parser SF
pair = do
    char '['
    l <- sf
    char ','
    r <- sf
    char ']'
    return $ Pair l r

sf :: Parser SF
sf = rn <|> pair

readIt :: Either a b -> b
readIt (Right x) = x

prettyRead :: String -> SF
prettyRead xs = readIt $ parse sf "" xs

mag :: SF -> Int
mag (RN n) = n
mag (Pair l r) = 3 * (mag l) + 2 * (mag r)

mags :: SF -> (Int, Int)
mags (RN n) = (n `div` 2, (succ n) `div` 2)
mags (Pair l r) = (mag l, mag r)

data Tr = L SF | R SF deriving Show

type Zipper = (SF, [Tr])

dig :: Zipper -> Maybe Zipper
dig (RN _, _) = Nothing
dig (Pair l r, ts) = case (length ts) of
    4 -> Just (Pair l r, ts)
    _ -> let ld = dig (l, (L r) :ts)
             rd = dig (r, (R l) :ts)
         in if isNothing ld then rd else ld

getSF :: Zipper -> SF
getSF (a, []) = a
getSF (l, (L r):ts) = getSF (Pair l r, ts)
getSF (r, (R l):ts) = getSF (Pair l r, ts)

replace :: a -> Maybe a -> a
replace x Nothing = x
replace _ (Just x) = x

rnL :: Zipper -> Zipper
rnL (RN n, ts) = (RN n, ts)
rnL (Pair l r, ts) = rnL (l, (L r):ts)

rnR :: Zipper -> Zipper
rnR (RN n, ts) = (RN n, ts)
rnR (Pair l r, ts) = rnR (r, (R l):ts)

rnToLeft :: Zipper -> Maybe Zipper
rnToLeft (_, []) = Nothing
rnToLeft (r, (R l):ts) = Just $ rnR (l, (L r):ts)
rnToLeft (l, (L r):ts) = rnToLeft (Pair l r, ts)

rnToRight :: Zipper -> Maybe Zipper
rnToRight (_, []) = Nothing
rnToRight (l, (L r):ts) = Just $ rnL (r, (R l):ts)
rnToRight (r, (R l):ts) = rnToRight (Pair l r, ts)

explode :: SF -> SF
explode a =
    let z = dig (a, [])
    in if isNothing z
        then a
        else let (b, ts) = replace (a, []) z
                 (l, r) = mags b
                 lre = do
                     (c, cts) <- rnToLeft (b, ts)
                     return $ getSF (RN (mag c + l), cts)
                 d = replace a lre
                 (_, ts') = replace (a, []) $ dig (d, [])
                 rre = do
                     (e, ets) <- rnToRight (RN 0, ts')
                     return $ getSF (RN (mag e + r), ets)
             in replace (getSF (RN 0, ts')) rre

big :: Zipper -> Maybe Zipper
big (RN n, ts)
    | n > 9 = Just (RN n, ts)
    | otherwise = Nothing
big (Pair l r, ts) =
    let bl = big (l, (L r):ts)
    in if isNothing bl
        then big (r, (R l):ts)
        else bl

split :: SF -> SF
split a =
    let z = big (a, [])
    in if isNothing z
        then a
        else let (b, ts) = replace (a, []) z
                 (l, r) = mags b
             in getSF (Pair (RN l) (RN r), ts)

reduce :: SF -> SF
reduce a =
    let a' = explode a
    in if a' /= a
        then reduce a'
        else let a'' = split a
        in if a'' /= a
            then reduce a''
            else a

add :: SF -> SF -> SF
add a b = reduce $ Pair a b

magSum :: [SF] -> [Int]
magSum sfs = do
    a <- sfs
    b <- sfs
    guard $ a /= b
    return $ mag $ add a b

main = do
    f <- readFile "input18.txt"
    let ls = lines f
    let sfs = map prettyRead ls
    putStrLn $ show $ maximum $ magSum sfs
