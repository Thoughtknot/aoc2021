{-# LANGUAGE BangPatterns #-}
module Day19 where
import Data.Functor
import Data.List.Split
import Day18 (parseLine)
import Data.Either (partitionEithers)
import Data.List
import Debug.Trace
import Data.Maybe
import qualified Data.Set as Set

type Point = (Int, Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day19.txt" <&> lines
    let l = parseLines 0 [] (tail ls)
    let f = let (i,j) = head l in (i,(0,0,0),j)
    let result = tryFitAll [f] (tail l)
    let rc = Set.unions $ map (\(a,b,c)->c) result
    print $ "Part 1: " ++ show (Set.size rc)
    let result' = getMaximum (map (\(a,b,c)->b) result)
    print $ "Part 2: " ++ show result'
    print "Done"

getMaximum :: [Point] -> Int
getMaximum l = maximum [abs (xx-yx) + abs (xy-yy) + abs (xz - yz)|(xx,xy,xz)<-l, (yx,yy,yz)<-l, (xx,xy,xz)/=(yz,yy,yz)]

tryFitAll :: [(Int, Point, Set.Set Point)] -> [(Int, Set.Set Point)] -> [(Int, Point, Set.Set Point)]
tryFitAll !h [] = h
tryFitAll !h !l = trace (show (fk, length h, length l)) tryFitAll (fit ++ h) (filter (\(a,b) -> a `notElem` fk) l)
    where
        fit = tryFitSingle h l Set.empty
        fk = map (\(a,b,c) -> a) fit

tryFitSingle :: [(Int, Point, Set.Set Point)] -> [(Int, Set.Set Point)] -> Set.Set Int -> [(Int, Point, Set.Set Point)]
tryFitSingle [] _ _ = []
tryFitSingle ((i,j,k):xs) l seen 
    | Set.member i seen = tryFitSingle xs l seen
    | otherwise = val ++ tryFitSingle xs l ns
        where 
            ns = Set.insert i seen
            val = map fromJust $! filter (/= Nothing) [tryFit (i,k) b | b<-l]

tryFit :: (Int, Set.Set Point) -> (Int, Set.Set Point) -> Maybe (Int, Point, Set.Set Point)
tryFit (!s1,!a) (!s2,!b) = getFit v a b s2
    where
        v = [(ap, bp, rot) | ap <- Set.toList a, bp <- Set.toList b, rot <- getAllRotations ap bp]

getFit :: [(Point, Point, Point->Point)] -> Set.Set Point -> Set.Set Point -> Int -> Maybe (Int, Point, Set.Set Point)
getFit [] a b c = Nothing
getFit ((an,bn,rot):xs) a b c = if Set.size (Set.intersection a v) >= 12 then Just (c,rot (0,0,0),v) else getFit xs a b c
    where
        v = Set.map rot b
        
getAllRotations :: Point -> Point -> [Point -> Point]
getAllRotations (!x,!y,!z) !bp = [rotation rot | (n,rot)<-rotations]
    where
        rotation rot = \(!f,!g,!h) -> do
            let (!u,!v,!w) = rot bp
            let (!l,!m,!n) = rot (f,g,h)
            (l+x-u,m+y-v, n+z-w)

rotations :: [(String, Point -> Point)]
rotations = [
        ("None",            \(!x,!y,!z) -> (x,y,z)),
        ("90x",             \(!x,!y,!z) -> (x,-z,y)),
        ("180x",            \(!x,!y,!z) -> (x,-y,-z)),
        ("270x",            \(!x,!y,!z) -> (x,z,-y)),
        ("90y",             \(!x,!y,!z) -> (z,y,-x)),
        ("180y",            \(!x,!y,!z) -> (-x,y,-z)),
        ("270y",            \(!x,!y,!z) -> (-z,y,x)),
        ("90z",             \(!x,!y,!z) -> (y,-x,z)),
        ("180z",            \(!x,!y,!z) -> (-x,-y,z)),
        ("270z",            \(!x,!y,!z) -> (-y,x,z)),
        ("90x 90y",         \(!x,!y,!z) -> (y,-z,-x)),
        ("90x 90y 90x",     \(!x,!y,!z) -> (y,x,-z)),
        ("90x 90y 180x",    \(!x,!y,!z) -> (y,z,x)),
        ("90x 180y",        \(!x,!y,!z) -> (-x,-z,-y)),
        ("90x 270y",        \(!x,!y,!z) -> (-y,-z,x)),
        ("90x 90y 90z",     \(!x,!y,!z) -> (-z,-y,-x)),
        ("90x 90y 90z 90x", \(!x,!y,!z) -> (-z,x,-y)),
        ("90x 90z",         \(!x,!y,!z) -> (-z,-x,y)),
        ("90x 180z",        \(!x,!y,!z) -> (-x,z,y)),
        ("180x 90z",        \(!x,!y,!z) -> (-y,-x,-z)),
        ("270x 90y",        \(!x,!y,!z) -> (-y,z,-x)),
        ("90y 90x",         \(!x,!y,!z) -> (z,x,y)),
        ("90y 180x",        \(!x,!y,!z) -> (z,-y,x)),
        ("90z 90y",         \(!x,!y,!z) -> (z,-x,-y))
    ]

parseLines :: Int -> [Point] -> [String] -> [(Int, Set.Set Point)]
parseLines i pts [] = [(i, Set.fromList pts)]
parseLines i pts ("":xs) = (i,Set.fromList  pts): parseLines (i+1) [] (tail xs)
parseLines i pts (x:xs) = parseLines i (p:pts) xs
    where
        [a,b,c] = splitOn "," x
        p = (read a, read b, read c)