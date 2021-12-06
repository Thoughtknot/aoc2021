module Day6 where
import Data.Functor
import Data.List.Split

import qualified Data.Vector.Storable.Mutable as IOVector

type Fish = (Int,Int,Int,Int,Int,Int,Int,Int,Int)

run :: IO ()
run = do
    ls <- readFile "src/day6.txt" <&> splitOn "," <&> map read
    let res = iterate iteration ls
    let a = take 81 res
    print ("Part 1: " ++ show (length $ last a))
    let result = sumFish $ iterateParse' (parse ls) 256
    print ("Part 2: " ++ show result)
    print "Done"

parse :: [Int] -> Fish
parse ls = (0, ones, twos, threes, fours, fives, 0, 0, 0)
    where
        ones = length $ filter (==1) ls
        twos = length $ filter (==2) ls
        threes = length $ filter (==3) ls
        fours = length $ filter (==4) ls
        fives = length $ filter (==5) ls

sumFish :: Fish -> Int
sumFish (a,b,c,d,e,f,g,h,i) = a+b+c+d+e+f+g+h+i

iterateParse' :: Fish -> Int -> Fish
iterateParse' iv 0 = iv
iterateParse' (zs, os, ts, ths, fs, fvs, sx, sv, e) x = iterateParse' (os, ts, ths, fs, fvs, sx, sv + zs, e, zs) (x - 1)

iteration :: [Int] -> [Int]
iteration lf = newList ++ newFish
    where
        newFish = [ 8 | x <- lf, x == 0]
        newList = [ if x == 0 then 6 else x - 1 | x <- lf ]
 