import Data.List
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String (Parser)

type Rule = ((Char, Char), Char)
type Rules = M.Map (Char, Char) Char

(!) :: Rules -> (Char, Char) -> Char
m ! r = M.findWithDefault ' ' r m

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

rules :: Parser (String, Rules)
rules = do
    t <- template
    rs <- many rule
    return $ (t, M.fromList rs)

readIt :: Either a (String, Rules) -> (String, Rules)
readIt (Left _) = ("", M.empty)
readIt (Right x) = x

merge :: String -> String -> String
merge [x] "" = [x]
merge (x:xs) (y:ys) = x:y:(merge xs ys)

step :: Rules -> String -> String
step rm t =
    let ps = zip t (tail t)
        is = map ((!) rm) ps
    in filter (/=' ') $ merge t is

main = do
    f <- readFile "input14.txt"
    let (t, rm) = readIt $ parse rules "" f
    let ss = t : map (step rm) ss
    let ls = map length $ group $ sort $ ss !! 10
    putStrLn $ show $ maximum ls - minimum ls
