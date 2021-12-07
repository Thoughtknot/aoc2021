module Day7 where
import Data.Functor
import Data.List.Split
import Debug.Trace

run :: IO ()
run = do
    ls <- readFile "src/day7.txt" <&> splitOn "," <&> map read
    let results = getSmallest [1..maximum ls] ls []
    print ("Part 1: " ++ show (minimum results))
    let result' = getSmallest' [1..maximum ls] ls []
    print ("Part 2: " ++ show (minimum result'))
    print "Done"

getSmallest :: [Int] -> [Int] -> [Int] -> [Int]
getSmallest [] l o = o
getSmallest (x:xs) l o = getSmallest xs l (n:o)
    where
        n = sum $ map (\r -> abs $ r - x) l

getSmallest' :: [Int] -> [Int] -> [Int] -> [Int]
getSmallest' [] l o = o
getSmallest' (x:xs) l o = getSmallest' xs l (sum (map asum l):o)
    where
        asum r = do
            let ab = abs $ r - x
            div (ab * (ab + 1)) 2