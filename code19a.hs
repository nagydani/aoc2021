import Data.List
import qualified Data.Set as S
import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)

-- Linear algebra

cross :: [Int] -> [Int] -> [Int]
[a, b, c] `cross` [d, e, f] =
    [b*f - c*e, c*d - a*f, a*e - b*d]

add, sub :: [Int] -> [Int] -> [Int]
add xs = zipWith (+) xs
sub xs = zipWith (-) xs

scale :: Int -> [Int] -> [Int]
scale n = map (*n)

rot :: [[Int]] -> [Int] -> [Int]
rot m a = foldl1' add $ zipWith scale a m

shift :: Int -> [a] -> [a]
shift n xs = drop n xs ++ take n xs

orient :: [[[Int]]]
orient = do
    c <- [1, -1]
    s <- [0 .. 2]
    let x = shift s [c, 0, 0]
    d <- [1, -1]
    u <- [0, 1]
    let y = shift s $ 0 : shift u [d, 0]
    let z = x `cross` y
    return [x, y, z]

-- Parsing

nat :: Parser Int
nat = do
    xs <- many1 digit
    return (read xs)

neg :: Parser Int
neg = do
    char '-'
    n <- nat
    return (-n)

vec :: Parser [Int]
vec = do
    v <- (neg <|> nat) `sepBy1` (char ',')
    char '\n'
    return v

scanner :: Parser [[Int]]
scanner = do
    string "--- scanner "
    nat
    string " ---\n"
    many vec

puzzle :: Parser [[[Int]]]
puzzle = scanner `sepBy` (char '\n')

readIt :: Either a [b] -> [b]
readIt (Left _) = []
readIt (Right xs) = xs

-- Alignment

align :: S.Set [Int] -> [[Int]] -> [S.Set [Int]]
align aset bs = do
    rotation <- orient
    let rbs = map (rot rotation) bs
    let as = S.toList aset
    a <- as
    b <- rbs
    let disp = a `sub` b
    let bset = S.fromList (map (add disp) rbs)
    let uset = aset `S.union` bset
    guard $ S.size uset + 12 <= S.size aset + S.size bset
    return uset

extend :: S.Set [Int] -> [[[Int]]] -> S.Set [Int]
extend aset bss =
    let bs = head bss
        usets = align aset bs
        uset = if null usets then aset else head usets -- cut
        tbss = tail bss
    in if null tbss then uset else extend uset tbss

buildSet :: S.Set [Int] -> [[[Int]]] -> S.Set [Int]
buildSet aset bss =
    let uset = extend aset bss
    in if uset == aset then aset else buildSet uset bss

-- main

main = do
    f <- readFile "input19.txt"
    let (p:ps) = readIt $ parse puzzle "" f
    let uset = buildSet (S.fromList p) ps
    putStrLn $ show $ S.size uset
