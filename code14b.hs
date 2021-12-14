import Data.List
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String (Parser)

type Rule = ((Char, Char), Char)
type Polymer = M.Map (Char, Char) Integer

(!) :: Polymer -> (Char, Char) -> Integer
m ! r = M.findWithDefault 0 r m

tally :: Polymer -> Char -> Integer
tally p c =
    let lc = sum $ map ((!) p) $ zip (repeat c) ['A'..'Z']
        rc = sum $ map ((!) p) $ zip ['A'..'Z'] (repeat c)
    in (lc + rc + 1) `div` 2

add :: (Char, Char) -> Integer -> Polymer -> Polymer
add p i m = M.insert p (m!p + i) m

polymer :: String -> Polymer
polymer xs =
    let ps = zip xs (tail xs)
    in foldl' (\m p -> add p 1 m) M.empty ps

upd :: Polymer -> Polymer -> Rule -> Polymer
upd m m' ((a, b), c) =
     add (a, c) (m!(a, b)) $ add (c, b) (m!(a, b)) m'

step :: [Rule] -> Polymer -> Polymer
step rs m = foldl' (upd m) M.empty rs

template :: Parser String
template = do
    xs <- many letter
    string "\n\n"
    return xs

rule :: Parser Rule
rule = do
    a <- letter
    b <- letter
    string " -> "
    c <- letter
    char '\n'
    return ((a, b), c)

rules :: Parser (String, [Rule])
rules = do
    t <- template
    rs <- many rule
    return $ (t, rs)

readIt :: Either a (String, [Rule]) -> (String, [Rule])
readIt (Left _) = ("", [])
readIt (Right x) = x

main = do
    f <- readFile "input14.txt"
    let (t, rs) = readIt $ parse rules "" f
    let ss = (polymer t) : map (step rs) ss
    let s = ss !! 40
    let cs = filter (/=0) $ map (tally s) ['A'..'Z']
    putStrLn $ show $ maximum cs - minimum cs
