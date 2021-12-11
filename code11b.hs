import Data.List
import Data.List.Split
import Data.Char
import Data.Array
import Control.Monad
import Control.Monad.State.Lazy

type OctoIx = (Int,Int)

data OctoState = OctoState
    { energy :: Array OctoIx Int        
    , flashed :: Array OctoIx Bool } deriving (Show)

outOfBounds :: (OctoIx,OctoIx) -> OctoIx -> Bool
outOfBounds ((xMin,yMin),(xMax,yMax)) (x,y) =
    (x<xMin) || (x>xMax) || (y<yMin) || (y>yMax) 

nIx, neIx, eIx, seIx, sIx, swIx, wIx, nwIx :: OctoIx -> OctoIx
nIx (x,y) = (x,y-1)
neIx (x,y) = (x+1,y-1)
eIx (x,y) = (x+1,y)
seIx (x,y) = (x+1,y+1)
sIx (x,y) = (x,y+1)
swIx (x,y) = (x-1,y+1)
wIx (x,y) = (x-1,y)
nwIx (x,y) = (x-1,y-1)

inc :: OctoIx -> State OctoState ()
inc ix = do
    state <- get
    let e = energy state
    unless (outOfBounds (bounds e) ix) $
        put $ state { energy = e // [(ix,succ $ e ! ix)] }

flash :: OctoIx -> State OctoState ()
flash ix = do
    state <- get
    let e = (energy state) ! ix
    let f = (flashed state) ! ix
    unless (e <= 9 || f) $ do
        put $ state { flashed = (flashed state) // [(ix, True)] }
        inc $ nIx ix
        inc $ neIx ix
        inc $ eIx ix
        inc $ seIx ix
        inc $ sIx ix
        inc $ swIx ix
        inc $ wIx ix
        inc $ nwIx ix

flashAll :: State OctoState ()
flashAll = do
    state <- get
    let ixs = indices $ energy state
    foldl1' (>>) $ map flash ixs

flashCnt :: OctoState -> Int
flashCnt s = length $ filter id (elems $ flashed s)

step' :: OctoState -> OctoState
step' state =
    let n = flashCnt state
        state' = execState flashAll state
        n' = flashCnt state'
    in if n == n'
        then state'
        else step' state'

step :: ([[Int]],Int) -> ([[Int]],Int)
step (e,f) =
    let w = length $ head e
        s = toState $ map (map succ) e
        s' = step' s
        n = flashCnt s'
        es = elems $ energy s'
        fs = elems $ flashed s'
        e' = chunksOf w $ zipWith (\x y -> if y then 0 else x) es fs
    in (e',f + n)

toState :: [[Int]] -> OctoState
toState xs =
    let h = length xs
        w = length $ head xs
        b = ((0,0),(pred w,pred h))
    in OctoState (listArray b $ concat xs) (listArray b $ repeat False)

main = do
    f <- readFile "input11.txt"
    let s = map (map digitToInt) $ lines f :: [[Int]]
    let l = length $ concat s :: Int
    let ss = (s,0) : map step ss
    let fs = map snd ss
    let sfs = zipWith (-) (tail fs) $ fs
    putStrLn $ show $ succ $ length $ takeWhile (<100) sfs
