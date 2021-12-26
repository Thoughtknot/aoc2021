{-# LANGUAGE BangPatterns #-}
module Day21 where
import Data.Functor
import Data.Char (digitToInt)
import Debug.Trace
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Turn = Player1 | Player2 deriving (Ord,Show,Eq)
type State = (Int, Int, Int, Int, Turn)

run :: IO ()
run = do
    let dice = cycle [1..100]
    [p1,p2] <- readFile "src/day21.txt" <&> lines <&> map (digitToInt . last)
    let (a,b,c) = game p1 p2 dice 0 0 0
    let result = if a < b then a * c * 3 else b * c * 3
    putStrLn ("Part 1: " ++ show result)
    let (a', b', c') = game' Map.empty (p1,p2,0,0,Player1)
    putStrLn ("Part 2: " ++ show (max b' c'))
    putStrLn "Done"

game :: Int -> Int -> [Int] -> Int -> Int -> Int -> (Int,Int,Int)
game p1 p2 l s1 s2 rolls = do
    let (p1m, r) = splitAt 3 l
    let mv1 = mod (p1 + sum p1m - 1) 10 + 1
    let pts1 = s1 + mv1
    if pts1 >= 1000 then
        (pts1, s2,rolls+1)
    else do
        let (p2m, r2) = splitAt 3 r
        let mv2 = mod (p2 + sum p2m - 1) 10 + 1
        let pts2 = s2 + mv2
        if pts2 >= 1000 then
            (pts1, pts2,rolls+2)
        else
            game mv1 mv2 r2 pts1 pts2 (rolls+2)

getUniversePoints :: Int -> [(Int, Int)]
getUniversePoints 1 = [(4,1), (5,3), (6,6), (7,7), (8,6), (9,3), (10,1)]
getUniversePoints 2 = [(5,1), (6,3), (7,6), (8,7), (9,6), (10,3), (1,1)]
getUniversePoints 3 = [(6,1), (7,3), (8,6), (9,7), (10,6), (1,3), (2,1)]
getUniversePoints 4 = [(7,1), (8,3), (9,6), (10,7), (1,6), (2,3), (3,1)]
getUniversePoints 5 = [(8,1), (9,3), (10,6), (1,7), (2,6), (3,3), (4,1)]
getUniversePoints 6 = [(9,1), (10,3), (1,6), (2,7), (3,6), (4,3), (5,1)]
getUniversePoints 7 = [(10,1), (1,3), (2,6), (3,7), (4,6), (5,3), (6,1)]
getUniversePoints 8 = [(1,1), (2,3), (3,6), (4,7), (5,6), (6,3), (7,1)]
getUniversePoints 9 = [(2,1), (3,3), (4,6), (5,7), (6,6), (7,3), (8,1)]
getUniversePoints 10 = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
getUniversePoints _ = error "Unknown points"

runUniverses :: Map State (Int, Int) -> State -> [(Int, Int)] -> (Int, Int) -> (Map State (Int, Int), Int, Int)
runUniverses m (p1,p2,s1,s2,Player1) ((x,f):xs) (a,b) = do
    let (newm,a',b') = game' m (x,p2,s1+x,s2,Player2)
    runUniverses newm (p1,p2,s1,s2,Player1) xs (f*a'+a,f*b'+b)
runUniverses m (p1,p2,s1,s2,Player2) ((x,f):xs) (a,b) = do
    let (newm,a',b') = game' m (p1,x,s1,s2+x,Player1)
    runUniverses newm (p1,p2,s1,s2,Player2) xs (f*a'+a,f*b'+b)
runUniverses m _ [] (a,b) = (m, a, b)

game' :: Map State (Int, Int) -> State  -> (Map State (Int, Int),Int,Int)
game' m (p1,p2,s1,s2,t) =
    case (m !? (p1,p2,s1,s2,t), t) of
        (Just (x,y), _) -> (m, x, y)
        (Nothing, Player1) -> do
            if s1 >= 21 then
                (m,1,0)
            else if s2 >= 21 then
                (m,0,1)
            else do
                let k = getUniversePoints p1
                let (newm,a',b') = runUniverses m (p1,p2,s1,s2,t) k (0,0)
                (Map.insert (p1,p2,s1,s2,t) (a',b') newm, a', b')
        (Nothing, Player2) -> do
            if s1 >= 21 then
                (m,1,0)
            else if s2 >= 21 then
                (m,0,1)
            else do
                let k = getUniversePoints p2
                let (newm,a',b') = runUniverses m (p1,p2,s1,s2,t) k (0,0)
                (Map.insert (p1,p2,s1,s2,t) (a',b') newm, a', b')
