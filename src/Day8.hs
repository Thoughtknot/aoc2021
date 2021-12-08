module Day8 where
import Data.Functor
import Data.List.Split
import Debug.Trace
import qualified Data.Set as Set
import Data.Map ((!))
import qualified Data.Map as Map

type Output = (Int, Int, Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day8.txt" <&> lines <&> map maplines
    let results = map snd ls >>= filter (\x -> length x == 2 || length x == 3 || length x == 4 || length x == 7)
    print ("Part 1: " ++ show (length results))
    let result' = map maplines' ls
    print ("Part 2: " ++ show (sum result'))
    print "Done"

maplines :: String -> ([String], [String])
maplines l = do
    let [a,b] = splitOn "|" l
    (words a, words b)

maplines' :: ([String], [String]) -> Int
maplines' (input,output) = do
    let mp = getMap
    let one = head $ filter ((==2) . length) input
    let seven = head $ filter ((==3) . length) input
    let four = head $ filter ((==4) . length) input
    let eight = head $ filter ((==7) . length) input
    let cf = one
    let tot = foldl1 (++) input
    let a = head [c | c <- seven, c `notElem` one]
    let flt l = [c | c <- "abcdefg", c /= a, length (filter (==c) tot) == l]
    let (b,c,e,f) = (head $ flt 6, head $ flt 8, head $ flt 4, head $ flt 9)
    let d = head [r | r <- four, r `notElem` [b,c,f]]
    let g = head [r | r <- eight, r `notElem` [a,b,c,d,e,f]]
    let mapint c = mp ! Set.fromList c
    let m = map (mapint . map (rpl (a,b,c,d,e,f,g))) output
    read (concat m)

rpl :: (Char, Char, Char, Char, Char, Char, Char) -> Char -> Char
rpl (a,b,c,d,e,f,g) v
    | v == a = 'a'
    | v == b = 'b'
    | v == c = 'c'
    | v == d = 'd'
    | v == e = 'e'
    | v == f = 'f'
    | v == g = 'g'
    | otherwise = error $ "unknown char: " ++ [v]

getMap :: Map.Map (Set.Set Char) String
getMap = Map.fromList [(zs, "0"), (os, "1"), (ts, "2"), (ths, "3"), (fs, "4"), (fvs, "5"), (sx, "6"), (sv, "7"), (ei, "8"), (nn, "9")]
    where
        zs = Set.fromList "abcefg"
        os = Set.fromList "cf"
        ts = Set.fromList "acdeg"
        ths = Set.fromList "acdfg"
        fs = Set.fromList "bcdf"
        fvs = Set.fromList "abdfg"
        sx = Set.fromList "abdefg"
        sv = Set.fromList "acf"
        ei = Set.fromList "abcdefg"
        nn = Set.fromList "abcdfg"
