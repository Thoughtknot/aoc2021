module Day20 where
import Data.Functor
import Data.ByteString.Char8 (ByteString, pack,index)
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.Maybe
import Data.Char
import Debug.Trace

type Point = (Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day20.txt" <&> lines
    let (al, m) = parse ls
    let stepped = steps 2 al m
    let result = length $ filter (== True) (Map.elems stepped)
    putStrLn ("Part 1: " ++ show result)
    let stepped' = steps 50 al m
    let result' = length $ filter (== True) (Map.elems stepped')
    putStrLn ("Part 2: " ++ show result')
    print "Done"

steps :: Int -> ByteString -> Map Point Bool -> Map Point Bool
steps 0 bs m = m
steps i bs m = steps (i-1) bs newm
    where
        ks = Map.keys m
        (maxx, minx) = (maximum $ map fst ks, minimum $ map fst ks)
        (maxy, miny) = (maximum $ map snd ks, minimum $ map snd ks)
        def = index bs 0 /= '.' && odd i
        newm = step def (minx, miny) (maxx, maxy) bs m

step :: Bool -> Point -> Point -> ByteString -> Map Point Bool -> Map Point Bool
step def (minx, miny) (maxx, maxy) al m =
    Map.fromList [((x,y), mapPoint def x y al m)|x<-[minx-1..maxx+1], y<-[miny-1..maxy+1]]

mapPoint :: Bool -> Int -> Int -> ByteString -> Map Point Bool -> Bool
mapPoint def x y al m = index al l == '#'
    where
        l = foldl (\acc x -> acc * 2 + x) 0 $
            map ((\x -> if x then 1 else 0) . fromMaybe def)
            [
                m !? (x-1,y-1), m !? (x,y-1), m !? (x+1,y-1),
                m !? (x-1,y), m !? (x,y), m !? (x+1,y),
                m !? (x-1,y+1), m !? (x,y+1), m !? (x+1,y+1)
            ]

parse :: [String] -> (ByteString, Map Point Bool)
parse ls = (bs, m)
    where
        (bs,r) = parseAlgorithm ls ""
        m = parseImage Map.empty r 0

parseImage :: Map Point Bool -> [[Char]] -> Int -> Map Point Bool
parseImage m [] _ = m
parseImage m (x:xs) y = parseImage (parseLine m y 0 x) xs (y+1)

parseLine :: Map Point Bool -> Int -> Int -> [Char] -> Map Point Bool
parseLine m y x [] = m
parseLine m y x (c:xs) = parseLine newm y (x+1) xs
    where
        newm = Map.insert (x,y) (c == '#') m

parseAlgorithm :: [String] -> String -> (ByteString, [String])
parseAlgorithm [] f = error "Empty list"
parseAlgorithm ("":xs) f = (pack f,xs)
parseAlgorithm (x:xs) f = parseAlgorithm xs (f ++ x)