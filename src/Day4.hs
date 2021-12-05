module Day4 where

import Data.Functor ((<&>))
import Data.List.Split
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Debug.Trace (trace)

type Square = [(Bool, Int)]

run :: IO ()
run = do
    ls <- readFile "src/day4.txt" <&> lines
    let numbers = map (\x -> read x :: Int) $ splitOn "," (head ls)
    let bingos = parseSquares (tail ls) []
    print (map (Matrix.fromList 5 5) bingos)
    let p = playBingo numbers bingos
    let result = filter (hasBingo 0) (snd p)
    let h = sum . map snd . filter (not . fst) $ head result
    print ("Part 1: " ++ show (h * fst p))
    let los = playLosingBingo numbers bingos
    print los
    let r = sum . map snd . filter (not . fst) $ head (snd los)
    print ("Part 2: " ++ show (fst los * r, r, fst los))
    print "Done"

playBingo :: [Int] -> [Square] -> (Int, [Square])
playBingo [] sqrs = (-1,sqrs)
playBingo (x:xs) sqrs
    | hasBingoFold newsqrs = (x,newsqrs)
    | otherwise = playBingo xs newsqrs
    where
        newsqrs = map (markSqr x) sqrs

playLosingBingo :: [Int] -> [Square] -> (Int, [Square])
playLosingBingo [] sqrs = (-1, sqrs)
playLosingBingo (x:xs) sqrs
    | all (hasBingo 0) newsqrs = (x, newsqrs)
    | otherwise = playLosingBingo xs (filter (not . hasBingo 0 ) newsqrs)
    where
        newsqrs = map (markSqr x) sqrs

hasBingoFold :: [Square] -> Bool
hasBingoFold = foldr ((||) . hasBingo 0) False

hasBingo :: Int -> Square -> Bool
hasBingo 5 s = False
hasBingo i s = bingo || hasBingo (i + 1) s
    where
        bingoCol = filter fst [s !! i, s !! (i + 5), s !! (i + 10), s !! (i + 15), s !! (i + 20)]
        bingoRow = filter fst [s !! (i*5), s !! (i*5+1), s !! (i*5+2), s !! (i*5+3), s !! (i*5+4)]
        bingo = length bingoCol == 5 || length bingoRow == 5

markSqr :: Int -> Square -> Square
markSqr v s = [if snd x == v then (True, v) else x | x <- s]

parseSquares :: [String] -> [Square] -> [Square]
parseSquares [] acc = acc
parseSquares ls acc = do
    let rows = map (\x -> (False, read x :: Int)) $ tail (take 6 ls) >>= words
    parseSquares (drop 6 ls) (rows:acc)