import Data.List
import Data.Array
import qualified Data.Set as S
import qualified Data.IntSet as I
import Text.Parsec
import Text.Parsec.String (Parser)

nat :: Parser Int
nat = do
    ds <- many1 digit
    return (read ds)

neg :: Parser Int
neg = do
    char '-'
    n <- nat
    return (-n)

num :: Parser Int
num = neg <|> nat

type Range = (Int, Int)

numRange :: Parser Range
numRange = do
    a <- num
    string ".."
    b <- num
    return (a, b)

type Cuboid = (Bool, Range, Range, Range)

cuboid :: Parser Cuboid
cuboid = do
    sw <- try (string "on") <|> string "off"
    string " x="
    x <- numRange
    string ",y="
    y <- numRange
    string ",z="
    z <- numRange
    char '\n'
    return (sw == "on", x, y, z)

puzzle :: Parser [Cuboid]
puzzle = many1 cuboid

readIt :: Either a [b] -> [b]
readIt (Left _) = []
readIt (Right xs) = xs

isect :: Range -> Range -> Bool
isect (al, ah) (bl, bh) =
    bl <= ah && al <= bh

type Delimit = (I.IntSet, I.IntSet, I.IntSet)

delimit :: Cuboid -> Delimit -> Delimit
delimit (_, (xl, xh), (yl, yh), (zl, zh)) (xs, ys, zs) =
    ( I.insert xl $ I.insert (succ xh) xs
    , I.insert yl $ I.insert (succ yh) ys
    , I.insert zl $ I.insert (succ zh) zs)

-- binary search in an ordered array
indexOf :: Array Int Int -> Int -> Int
indexOf a n = bsearch (bounds a) where
    bsearch (l,h) =
        let m = (l + h) `div` 2
            am = a ! m
        in if am == n
            then m
            else if am < n
                then bsearch (m + 1, h)
                else bsearch (l, m - 1)

rTrans :: Array Int Int -> Range -> Range
rTrans a (xl, xh) = (indexOf a xl, indexOf a (xh + 1) - 1)

type Point = (Int, Int, Int)
type CMap = (Array Int Int, Array Int Int, Array Int Int)

rebootStep :: CMap -> Cuboid -> (S.Set Point -> S.Set Point)
rebootStep (ax, ay, az) (sw, rx, ry, rz) =
    let (xl, xh) = rTrans ax rx
        (yl, yh) = rTrans ay ry
        (zl, zh) = rTrans az rz
        cuboid = S.fromList $ range ((xl, yl, zl), (xh, yh, zh))
    in if sw
        then S.union cuboid
        else (S.\\ cuboid)

reboot :: CMap -> [Cuboid] -> (S.Set Point -> S.Set Point)
reboot cm cs = foldl1' (flip (.)) (map (rebootStep cm) cs)

oCuboid :: CMap -> Point -> Cuboid
oCuboid (ax, ay, az) (x, y, z) =
    let xr = (ax!x, ax!(x + 1) - 1)
        yr = (ay!y, ay!(y + 1) - 1)
        zr = (az!z, az!(z + 1) - 1)
    in (True, xr, yr, zr)

addCuboid :: [Cuboid] -> Cuboid -> [Cuboid]
addCuboid cs (True, rx, ry, rz) =
    let (is, ds) = partition
            (\(_, rx', ry', rz')
                -> isect rx rx' && isect ry ry' && isect rz rz') cs
    in if null is
        then (True, rx, ry, rz) : ds
        else let cs = is ++ [(True, rx, ry, rz)]
                 (xs, ys, zs) = foldr delimit (I.empty, I.empty, I.empty) cs
                 cm = ( listArray (1, I.size xs) $ I.toAscList xs
                      , listArray (1, I.size ys) $ I.toAscList ys
                      , listArray (1, I.size zs) $ I.toAscList zs )
                 sc = (reboot cm cs) S.empty
                 is' = map (oCuboid cm) (S.toList sc)
             in ds ++ is'

subCuboid :: [Cuboid] -> Cuboid -> [Cuboid]
subCuboid cs (False, rx, ry, rz) =
    let (is, ds) = partition
            (\(_, rx', ry', rz')
                -> isect rx rx' && isect ry ry' && isect rz rz') cs
    in if null is
        then ds
        else let cs = is ++ [(False, rx, ry, rz)]
                 (xs, ys, zs) = foldr delimit (I.empty, I.empty, I.empty) cs
                 cm = ( listArray (1, I.size xs) $ I.toAscList xs
                      , listArray (1, I.size ys) $ I.toAscList ys
                      , listArray (1, I.size zs) $ I.toAscList zs )
                 sc = (reboot cm cs) S.empty
                 is' = map (oCuboid cm) (S.toList sc)
             in ds ++ is'

mergeCuboid :: [Cuboid] -> Cuboid -> [Cuboid]
mergeCuboid cs (t, rx, ry, rz) =
    (if t then addCuboid else subCuboid) cs (t, rx, ry, rz)

volume :: Cuboid -> Integer
volume (True, rx, ry, rz) = l rx * l ry * l rz
    where l (l, h) = fromIntegral (h + 1 - l)

main = do
    f <- readFile "input22.txt"
    let cs = readIt $ parse puzzle "" f
        cds = foldl' mergeCuboid [] cs
    putStrLn $ show $ sum $ map volume cds
