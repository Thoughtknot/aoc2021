module Day9 where
import Data.Functor
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Maybe
import qualified Data.Set as Set
import Data.Map ((!))
import GHC.OldList (sortBy)
import Data.List (sort)

run :: IO ()
run = do
    ls <- readFile "src/day9.txt" <&> lines
    let results = parseMap 0 ls Map.empty
    let ks = map fst $ Map.toList results
    let r = findLowest ks results []
    print $ "Part 1: " ++ show (sum $ map ((+1) . snd) r)
    let findBasins x = findBasin results x (Set.singleton x)
    let result' = take 3 $ sortBy (flip compare) [length $ findBasins (fst x) | x <- r]
    print $ "Part 2: " ++ show (result', product result')
    print "Done"

findBasin :: Map.Map (Int, Int) Int -> (Int, Int) -> Set.Set (Int, Int) -> [(Int, Int)]
findBasin m (xx, xy) seen = do
    let newS = [(x,y) | (x,y)<-[u,l,d,r], y /= 9, m ! (xx, xy) < y, not $ Set.member x seen]
    let newSeen =  Set.union seen $ Set.fromList (map fst newS)
    if null newS then
        Set.toList seen
    else
        Set.toList $ Set.fromList $ concatMap (\x -> findBasin m (fst x) newSeen) newS
    where
        u = ((xx, xy-1), fromMaybe 9 $ m Map.!? (xx, xy-1))
        d = ((xx, xy+1), fromMaybe 9 $ m Map.!? (xx, xy+1))
        l = ((xx-1, xy), fromMaybe 9 $ m Map.!? (xx-1, xy))
        r = ((xx+1, xy),  fromMaybe 9 $ m Map.!? (xx+1, xy))

findLowest :: [(Int, Int)] -> Map.Map (Int, Int) Int -> [((Int,Int),Int)] -> [((Int,Int),Int)]
findLowest [] m a = a
findLowest (x:xs) m a
    | isLowest x m = findLowest xs m ((x,m Map.! x):a)
    | otherwise = findLowest xs m a

isLowest :: (Int, Int) -> Map.Map (Int, Int) Int -> Bool
isLowest (xx, xy) m = xv < u && xv < d && xv < l && xv < r
    where
        xv = m Map.! (xx, xy)
        u = fromMaybe 9 $ m Map.!? (xx, xy-1)
        d = fromMaybe 9 $ m Map.!? (xx, xy+1)
        l = fromMaybe 9 $ m Map.!? (xx-1, xy)
        r = fromMaybe 9 $ m Map.!? (xx+1, xy)

parseMap :: Int -> [String] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
parseMap y [] m = m
parseMap y (x:xs) m = parseMap (y+1) xs (parseRow 0 y x m)

parseRow :: Int -> Int -> [Char] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
parseRow i y [] m = m
parseRow i y (x:xs) m = parseRow (i+1) y xs (Map.insert (i,y) (digitToInt x) m)