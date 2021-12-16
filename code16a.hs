import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char
import Data.List

hexToBin :: Char -> String
hexToBin = toBinary4 . digitToInt

toBinary :: Int -> String
toBinary 0 = ""
toBinary n = toBinary (n `div` 2) ++ if odd n then "1" else "0"

toBinary4 :: Int -> String
toBinary4 n =
    let b = toBinary n
    in (replicate (4 - length b) '0') ++ b

fromBinary :: String -> Int
fromBinary = foldl' (\a d -> a + a + digitToInt d) 0

int3 :: Parser Int
int3 = do
    v <- count 3 digit
    return $ fromBinary v

data Packet = Packet
    { version :: Int
    , packetId :: Int
    , payload :: Payload } deriving (Show)

data Payload = Lit Int | Op [Packet] deriving (Show)

subPackets :: Payload -> [Packet]
subPackets (Op xs) = xs

chunk :: Parser String
chunk = do
    c <- digit
    p <- count 4 digit
    if c == '0'
        then return p
        else do
            n <- chunk
            return $ p ++ n

readIt :: Either a [b] -> [b]
readIt (Left _) = []
readIt (Right xs) = xs

packet :: Parser Packet
packet = do
    v <- int3
    t <- int3
    if t == 4
        then do
            c <- chunk
            return $ Packet v t (Lit (fromBinary c))
        else do
            l <- digit
            if l == '0'
                then do
                    n <- count 15 digit
                    let len = fromBinary n
                    sub <- count len digit
                    let ps = readIt $ parse (many packet) "" sub
                    return $ Packet v t (Op ps)
                else do
                    n <- count 11 digit
                    let num = fromBinary n
                    ps <- count num packet
                    return $ Packet v t (Op ps)

sumVer :: Packet -> Int
sumVer p =
    let v = version p
        t = packetId p
    in if t == 4
        then v
        else
            let ps = subPackets $ payload p
            in v + (sum $ map sumVer ps)

readPacket :: Either a Packet -> Packet
readPacket (Right p) = p

main = do
    f <- readFile "input16.txt"
    let l = head $ lines f
    let b = concatMap hexToBin l
    let p = readPacket $ parse packet "" b
    putStrLn $ show $ sumVer p
