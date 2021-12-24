import Data.Array
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad

data Reg = W | X | Y | Z deriving (Eq, Ord, Enum, Show, Read, Ix)

type Val = Either Reg Int

regRead :: String -> Reg
regRead = read . (map toUpper) 

divZ :: Int -> Int -> Int
divZ a b =
    let (d, m) = divMod a b
    in if d < 0
        then if m == 0 then d else succ d
        else d

modZ :: Int -> Int -> Int
modZ a b = a - b * (a `divZ` b)

eql :: Int -> Int -> Int
eql a b = if a == b then 1 else 0

fnRead :: String -> (Int -> Int -> Int)
fnRead s = case s of
    "add" -> (+)
    "mul" -> (*)
    "div" -> divZ
    "mod" -> modZ
    "eql" -> eql

alu :: Array Reg Int -> [[String]] -> String -> Int
alu registers [] _ = registers ! Z
alu registers program input =
    let inst = head program
        op = head inst
        target = regRead (inst !! 1)
    in if op == "inp"
        then if null input
                then registers ! Z
                else alu (registers // [(target, digitToInt $ head input)]) (tail program) (tail input) 
        else let source = inst !! 2
             in let src = if isLetter (head source)
                             then registers ! (regRead source)
                             else read source
                    op1 = registers ! target
                    fn = fnRead op
                in alu (registers // [(target, fn op1 src)]) (tail program) input

start :: Array Reg Int
start = listArray (W,Z) (repeat 0)

zee :: Int -> Array Reg Int
zee n = listArray (W,Z) [0, 0, 0, n]

crack :: [[String]] -> Int -> String -> [Char]
crack pr n s =
    let z = alu start pr s
        x = (z `modZ` 26) + n
    in if x > 0 && x < 10
        then [intToDigit x]
        else []

iter :: [[String]] -> String -> [(String, Int)]
iter pr s = do
    let l = length s
    let prc = drop (18 * l) pr
    let n = read ((prc !! 5) !! 2 ) :: Int
    c <- if n >= 0
            then "123456789"
            else crack pr n s
    let s' = s ++ [c]
    return (s', alu start pr s')

app :: [[String]] -> String -> [String]
app pr s = map fst $ iter pr s

main = do
    f <- readFile "input24.txt"
    let pr = map words (lines f)
    let a = app pr
    let zs = a "" >>= a >>= a >>= a >>= a >>= a >>= a >>=
             a >>= a >>= a >>= a >>= a >>= a >>= (iter pr)
    let s = fst $ head $ filter (\(s, n) -> n == 0) zs
    putStrLn s
