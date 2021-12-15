{-# LANGUAGE BangPatterns #-}
module Day15 where
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (!?))
import Data.List
import Debug.Trace
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Bifunctor ( Bifunctor(second) )
import qualified GHC.Num as Math

type Pos = (Int, Int)
type Prev = Map.Map Pos Pos
type Dist = Map.Map Pos Int
type Vertices = Map.Map Pos Int

run :: IO ()
run = do
    ls <- readFile "src/day15.txt" <&> lines <&> map (map digitToInt)
    let q = parseStarting 0 ls []
    let m = Map.fromList q
    let (maxX,maxY) = let nodes = map fst q in (maximum $ map fst nodes, maximum $ map snd nodes)
    let dist = astar q (maxX,maxY)
    putStrLn ("Part 1: " ++ show (dist ! (maxX, maxY)))
    let bigls = repeatPattern ls
    let q' = parseStarting 0 bigls []
    let m' = Map.fromList q'
    let (maxX',maxY') = let nodes = map fst q' in (maximum $! map fst nodes, maximum $! map snd nodes)
    let dist' = astar q' (maxX',maxY')
    putStrLn ("Part 2: " ++ show (dist' ! (maxX', maxY')))
    putStrLn "Done"

repeatPattern :: [[Int]] -> [[Int]]
repeatPattern !ls = [concat $! repeatList l 5 0 | l<-ls]
    ++ [concat $! repeatList l 5 1 | l<-ls]
    ++ [concat $! repeatList l 5 2 | l<-ls]
    ++ [concat $! repeatList l 5 3 | l<-ls]
    ++ [concat $! repeatList l 5 4 | l<-ls]

repeatList :: [Int] -> Int -> Int -> [[Int]]
repeatList !ls 0 !o = []
repeatList ls c offset = [mapInc l (5 - c + offset) |l<-ls] : repeatList ls (c-1) offset

mapInc :: Int -> Int -> Int
mapInc !v 0 = v
mapInc !v !n
    | v + 1 > 9 = mapInc 1 (n-1)
    | otherwise = mapInc (v+1) (n-1)

astar :: [(Pos, Int)] -> (Int, Int) -> Dist
astar !q (!xe,!ye) = do
    let h (!xa,!ya) = Math.abs (xa - xe) + Math.abs (ya - ye)
    let m = Map.fromList q
    let prev = Map.empty
    let gscore = Map.fromList [if x == (0,0) then ((0,0),0) else (x,1000000000) | (x,v)<-q]
    let fscore = Map.fromList [if x == (0,0) then ((0,0),h (0,0)) else (x,1000000000) | (x,v)<-q]

    let openSet = [(0,0)]
    astarStep h (xe, ye) openSet m prev gscore fscore

astarStep :: (Pos -> Int) -> Pos -> [Pos] -> Vertices -> Prev -> Dist -> Dist -> Dist
astarStep !h !goal [] !vertices !prev !gscore !fscore = error "not implemented"
astarStep !h !goal os !vertices !prev !gscore !fscore = do
    let u = minimumBy (comparing (fscore !)) os
    if u == goal then
        gscore
    else do
        let ls = getNeighbors u vertices
        let (newgscore, newfscore, newprev, nos) = astarNeighbors os u ls h gscore fscore prev
        astarStep h goal (filter (/= u) nos) vertices newprev newgscore newfscore

astarNeighbors :: [Pos] -> Pos -> [(Pos, Int)] -> (Pos -> Int) -> Dist -> Dist -> Prev -> (Dist, Dist, Prev, [Pos])
astarNeighbors !os !cur [] !h !gscore !fscore !prev = (gscore, fscore, prev, os)
astarNeighbors !os !cur ((!x,!v):xs) !h !gscore !fscore !prev = astarNeighbors newos cur xs h newg newf newprev
    where
        tempg = gscore ! cur + v
        gv = gscore ! x
        newg = if tempg < gv then Map.insert x tempg gscore else gscore
        newf = if tempg < gv then Map.insert x (tempg + h x) fscore else fscore
        newprev = if tempg < gv then Map.insert x cur prev else prev
        newos = if tempg < gv && x `notElem` os then x:os else os

getNeighbors :: Pos -> Vertices -> [(Pos,Int)]
getNeighbors (x,y) !m = map (second fromJust) $! filter (isJust . snd) [
        ((x,y-1),m !? (x,y-1)),
        ((x,y+1),m !? (x,y+1)),
        ((x-1,y),m !? (x-1,y)),
        ((x+1,y),m !? (x+1,y))
    ]

parseStarting :: Int -> [[Int]] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
parseStarting y [] v = v
parseStarting y (x:xs) v = parseStarting (y+1) xs (parseRow y 0 x v)

parseRow :: Int -> Int -> [Int] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
parseRow y x [] r = r
parseRow y x (h:xs) r = parseRow y (x+1) xs (((x,y),h):r)