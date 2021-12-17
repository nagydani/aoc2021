import Data.List
import Control.Monad

type Vec2d = (Int, Int)
type Probe = (Vec2d, Vec2d)

-- target area
left = 201 :: Int
right = 230 :: Int
top = (-65) :: Int
bottom = (-99) :: Int

update :: Probe -> Probe
update ((xp, yp), (xv, yv)) =
    ((xp + xv, yp + yv), (xv - signum xv, yv - 1))

shot :: Vec2d -> [Vec2d]
shot (xv, yv) =
    let ps = ((0, 0), (xv, yv)) : map update ps
    in takeWhile (\(x, y) -> x <= right && y >= bottom) $ map fst ps

onTarget :: [Vec2d] -> Bool
onTarget ps =
    let (x, y) = last ps
    in x >= left && y <= top

highPoint :: [Vec2d] -> Int
highPoint ps = maximum $ map snd ps

shoot :: [Int]
shoot = do
    xv <- [10..right]
    yv <- [top..(-bottom)]
    let ps = shot (xv, yv)
    guard $ onTarget ps
    return $ highPoint ps

main = putStrLn $ show $ maximum shoot
