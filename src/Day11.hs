module Day11 where
import Data.Functor
import Debug.Trace
import Data.List (sort)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import qualified Data.Set as Set

run :: IO ()
run = do
    ls <- readFile "src/day11.txt" <&> lines <&> map (map digitToInt)
    let k = Map.fromList $ parseStarting 0 ls []
    let (flashes, newM) = steps 100 (0, k)
    putStrLn ("Part 1: " ++ show flashes)
    let allFlashStep = findAllFlash 0 (Map.size k) k
    putStrLn ("Part 2: " ++ show allFlashStep)
    print "Done"

findAllFlash :: Int -> Int -> Map.Map (Int, Int) Int -> Int
findAllFlash step len m = do
    let ns = step + 1
    let (nflash, newm) = singleStep m
    if nflash == len then
        ns
    else
        findAllFlash ns len newm  

steps :: Int -> (Int, Map.Map (Int, Int) Int) -> (Int, Map.Map (Int, Int) Int)
steps 0 (a,m) = (a,m)
steps flashes (v,m) = steps (flashes-1) (na+v,newm)
    where
        (na,newm) = singleStep m

singleStep :: Map.Map (Int, Int) Int -> (Int, Map.Map (Int, Int) Int)
singleStep m = do
    let (flash, newm) = Map.mapAccumWithKey increase [] m
    let newnewm = handleFlashes flash newm
    Map.mapAccum (\a x -> if x > 9 then (a+1, 0) else (a, x)) 0 newnewm

handleFlashes :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
handleFlashes keys m = do
    let triggered = concatMap getAdjacent keys
    let (flash, newm) = increaseTriggered triggered [] m
    if null flash then
        newm
    else
        handleFlashes flash newm

increaseTriggered :: [(Int, Int)] -> [(Int, Int)] -> Map.Map (Int, Int) Int -> ([(Int, Int)], Map.Map (Int, Int) Int)
increaseTriggered [] acc m = (acc, m)
increaseTriggered (x:xs) acc m = increaseTriggered xs newacc newm
    where
        (v, newm) = Map.updateLookupWithKey (\k x -> Just (x + 1)) x m
        newacc = case v of
            Nothing -> acc
            Just nv -> if nv == 10 then x:acc else acc

getAdjacent :: (Int, Int) -> [(Int, Int)]
getAdjacent (x,y) = [(x-1,y-1), (x-1,y), (x-1,y+1), (x, y-1), (x,y+1), (x+1,y-1), (x+1,y), (x+1,y+1)]

increase :: [(Int, Int)] -> (Int, Int) -> Int -> ([(Int, Int)], Int)
increase acc k v
    | v == 9 = (k:acc, v + 1)
    | otherwise = (acc, v + 1)

parseStarting :: Int -> [[Int]] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
parseStarting y [] v = v
parseStarting y (x:xs) v = parseStarting (y+1) xs (parseRow y 0 x []++v)

parseRow :: Int -> Int -> [Int] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
parseRow y x [] r = r
parseRow y x (h:xs) r = parseRow y (x+1) xs (((x,y),h):r)