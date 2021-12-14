{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day14 where
import Data.Functor
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List
import Debug.Trace

type Template = String

run :: IO ()
run = do
    (rules, tmp) <- readFile "src/day14.txt" <&> lines <&> parseLines Map.empty
    let result = iterate (\s -> applyRule s rules []) tmp !! 10
    let c = sort $ map length $ group $ sort result
    putStrLn ("Part 1: " ++ show (last c - head c))
    let ir = iterate (\s -> applyRule' (Map.toList s) rules Map.empty) (countPairs tmp Map.empty) !! 40
    let result' = sort . map snd $ Map.toList $ countIndividualChars (Map.toList ir) Map.empty
    putStrLn ("Part 2: " ++ show (last result' - head result'))
    putStrLn "Done"

countIndividualChars :: [(String, Int)] -> Map.Map Char Int -> Map.Map Char Int
countIndividualChars [] m = m
countIndividualChars (([a,b],v):xs) m = countIndividualChars xs $ Map.insertWith (+) b v m

applyRule' :: [(String, Int)] -> Map.Map String String -> Map.Map String Int -> Map.Map String Int
applyRule' [] m n = n
applyRule' ((p,v):xs) m n = applyRule' xs m $ Map.insertWith (+) [a,k] v (Map.insertWith (+) [k,b] v n)
    where
        [a,b] = p
        [k] = m ! p

countPairs :: Template -> Map.Map String Int -> Map.Map String Int
countPairs [x] m = m
countPairs (x:y:xs) m = countPairs (y:xs) $ Map.insertWith (+) [x,y] 1 m

applyRule :: Template -> Map.Map String String -> Template -> Template
applyRule [x] m n = reverse (x:n)
applyRule (x:xs) m n = x:m ! [x, head xs] ++ applyRule xs m n

parseLines :: Map.Map String String -> [String] -> (Map.Map String String, Template)
parseLines m ls = (foldl parsePairInsertion Map.empty (tail . tail $ ls), head ls)
    where
        parsePairInsertion m s = let [a,_,b] = words s in Map.insert a b m
        

