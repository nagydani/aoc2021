import Data.List
import Data.Array
import qualified Data.Set as S
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

type Point = (Int, Int, Int)

rebootStep :: Cuboid -> (S.Set Point -> S.Set Point)
rebootStep (sw, (xl, xh), (yl, yh), (zl, zh)) =
    let cuboid = S.fromList $ range ((xl, yl, zl), (xh, yh, zh))
    in if sw
        then S.union cuboid
        else (S.\\ cuboid)

reboot :: [Cuboid] -> (S.Set Point -> S.Set Point)
reboot cs = foldl1' (flip (.)) (map rebootStep cs)

inRng :: Range -> Range -> Bool
inRng (rl, rh) (xl, xh) = xl >= rl && xh <= rh

cuboidInRange :: Range -> Cuboid -> Bool
cuboidInRange r (_, x, y, z) = ir x && ir y && ir z where ir = inRng r

main = do
    f <- readFile "input22.txt"
    let ir = inRange (-50, 50)
    let cs = filter (cuboidInRange (-50, 50)) $ readIt $ parse puzzle "" f
    let sc = (reboot cs) S.empty
    putStrLn $ show $ S.size sc
