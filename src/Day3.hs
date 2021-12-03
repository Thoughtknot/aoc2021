module Day3 where
import Data.Functor ((<&>))
import qualified Data.Map as Map hiding (map, foldl)
import Data.Char (digitToInt)
import Data.Maybe ( listToMaybe, fromJust )
import Numeric (readInt)

run :: IO ()
run = do
    ls <- readFile "src/day3.txt" <&> lines
    let mc = foldl (mostCommonIn 0) Map.empty ls
    let result = map snd (Map.toAscList mc)
    print ("Part 1: " ++ show (binaryToDec (map gamma result) * binaryToDec (map epsilon result)))
    let ogr = getRating mostCommon ls 0
    let cosr = getRating leastCommon ls 0
    print ("Part 2: " ++ show (binaryToDec ogr * binaryToDec cosr))
    print "Done"

getRating :: (Int -> [String] -> Char) -> [String] -> Int -> String
getRating f [] bp = error "Empty list!"
getRating f [x] bp = x
getRating f ls bp = getRating f (filterOn flt bp ls) (bp + 1)
    where
        flt = f bp ls

filterOn :: Char -> Int -> [String] -> [String]
filterOn f bp ls = [s | s <- ls, s !! bp == f]

mostCommon ::Int -> [String] -> Char
mostCommon bp ls = if count '1' sbts >= count '0' sbts then '1' else '0'
    where
        sbts = map (!! bp) ls
        
leastCommon ::Int -> [String] -> Char
leastCommon bp ls = if count '0' sbts <= count '1' sbts then '0' else '1'
    where
        sbts = map (!! bp) ls

count :: Char -> [Char] -> Int
count x = length . filter (x==)

gamma :: Int -> Char
gamma i
    | i > 0 = '1'
    | i < 0 = '0'
    | otherwise = error "No bit is more common"

epsilon :: Int -> Char
epsilon i
    | i < 0 = '1'
    | i > 0 = '0'
    | otherwise = error "No bit is less common"

binaryToDec :: String -> Int
binaryToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

mostCommonIn :: Int -> Map.Map Int Int -> String-> Map.Map Int Int
mostCommonIn _ m [] = m
mostCommonIn i m ('1':xs) = mostCommonIn (i + 1) (Map.insertWith (+) i 1 m) xs
mostCommonIn i m ('0':xs) = mostCommonIn (i + 1) (Map.insertWith (+) i (-1) m) xs
mostCommonIn _ _ (x:xs) = error $ "Unknown bit " ++ show x


