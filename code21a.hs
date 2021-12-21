import Control.Monad
import Control.Monad.State.Lazy

data Game = Game
    { pl1 :: (Int, Int)
    , pl2 :: (Int, Int)
    , dierolls :: Int } deriving (Show)

start = Game
    { pl1 = (6, 0)
    , pl2 = (2, 0)
    , dierolls = 0 }

roll :: State Game Int
roll = do
    state <- get
    let r = dierolls state
    put state { dierolls = succ r }
    return (1 + r `mod` 100)

move :: Int -> State Game Int
move r = do
    state <- get
    let pl = odd $ dierolls state
    let (pos, score) = if pl
        then pl1 state
        else pl2 state
    let newPos = 1 + (pos + r - 1) `mod` 10
    let newScore = score + newPos
    put $ if pl
        then state { pl1 = (newPos, newScore) }
        else state { pl2 = (newPos, newScore) }
    return $ if newScore < 1000
        then 0
        else dierolls state * if pl
            then snd $ pl2 state
            else snd $ pl1 state

turn :: State Game Int
turn = do
    r1 <- roll
    r2 <- roll
    r3 <- roll
    move $ r1 + r2 + r3

game :: State Game Int
game = do
    res <- turn
    if (res == 0)
        then game
        else return res

main :: IO ()
main = do
    let res = evalState game start
    putStrLn $ show res
