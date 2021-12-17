module Day17 where
import Data.Functor
import Data.List.Split.Internals (splitOn)
import Debug.Trace

type Range = (Int, Int)
type Pos = (Int, Int)
type Vel = (Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day17.txt"
    let target = parseTarget ls
    let sims = [simulate (0,0) (x,y) target 0|x<-[0..1000],y<-[-1000..1000]]
    let result = maximum sims
    putStrLn $ "Part 1: " ++ show result
    let result' = filter (/= -1) sims
    putStrLn $ "Part 2: " ++ show (length result')
    putStrLn "Done"

simulate :: Pos -> Vel -> (Range, Range) -> Int -> Int
simulate (x,y) (xv, yv) ((xm, xx), (ym, yx)) ymax
    | x > xx || (xv == 0  && x < xm) || y < ym = -1
    | x >= xm && x <= xx && y >= ym && y <= yx = ymax
    | otherwise = simulate (x+xv, y+yv) (nxv, yv - 1) ((xm, xx), (ym, yx)) nymax
    where
        nxv = if xv == 0 then 0 else xv - 1
        nymax = if y > ymax then y else ymax

parseTarget :: String -> (Range, Range)
parseTarget s = do
    let [xi,yi] = splitOn ", y=" (drop 15 s)
    let [xl, xh] = splitOn ".." xi
    let [yl, yh] = splitOn ".." yi
    ((read xl, read xh), (read yl, read yh))