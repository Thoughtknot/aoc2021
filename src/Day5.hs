module Day5 where
import Data.Functor
import Data.List.Split.Internals (splitOn)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map

type Coord = (Int,Int)

run :: IO ()
run = do
    ls <- readFile "src/day5.txt" <&> lines
    let vents = parseCoords [] ls
    let countOverlaps = foldl (foldl (\ m x -> Map.insertWith (+) x 1 m)) Map.empty
    let resMap = countOverlaps vents
    let res1 = length . filter (>1) $ map snd $ Map.toList resMap
    print ("Part 1: " ++ show res1)
    let vents' = parseCoords' [] ls
    let resMap = countOverlaps vents'
    let res2 = length . filter (>1) $ map snd $ Map.toList resMap
    print ("Part 2: " ++ show res2)
    print "Done"

parseCoords :: [[Coord]] -> [String] -> [[Coord]]
parseCoords = foldl (\c x -> parseVent (words x) : c)

parseCoords' :: [[Coord]] -> [String] -> [[Coord]]
parseCoords' = foldl (\c x -> parseVent' (words x) : c)

(.+.) :: Coord -> Coord -> Coord
(ax,ay) .+. (bx,by) = (ax+bx,ay+by) 

parseVent' :: [String] -> [Coord]
parseVent' ls
    | sx == ex = [(sx,y)|y<-[sy..ey]]
    | sy == ey = [(x,sy)|x<-[sx..ex]]
    | ax < bx && ay < by = [(ax,ay).+.(i,i)|i<-[0..(bx-ax)]]
    | ax < bx && ay > by = [(ax,ay).+.(i,-i)|i<-[0..(bx-ax)]]
    | ax > bx && ay > by = [(ax,ay).+.(-i,-i)|i<-[0..(ax-bx)]]
    | ax > bx && ay < by = [(ax,ay).+.(-i,i)|i<-[0..(ax-bx)]]
    | otherwise = error "Unknown constraints"
    where
        getCoords ls = map (\x -> read x :: Int) $ splitOn "," ls
        (sx,ex) = if ax < bx then (ax,bx) else (bx,ax) 
        (sy,ey) = if ay < by then (ay,by) else (by,ay)
        [ax,ay] = getCoords (head ls)
        [bx,by] = getCoords (last ls)

parseVent :: [String] -> [Coord]
parseVent ls
    | sx == ex = [(sx,y)|y<-[sy..ey]]
    | sy == ey = [(x,sy)|x<-[sx..ex]]
    | otherwise = []
    where
        getCoords ls = map (\x -> read x :: Int) $ splitOn "," ls
        (sx,ex) = if ax < bx then (ax,bx) else (bx,ax) 
        (sy,ey) = if ay < by then (ay,by) else (by,ay)
        [ax,ay] = getCoords (head ls)
        [bx,by] = getCoords (last ls)

-- 18118 too low