{-# LANGUAGE BangPatterns #-}
module Day12 where
import Data.Functor
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Map ((!), (!?))
import GHC.Unicode (isUpper)
import Debug.Trace (trace)
import GHC.OldList (intercalate)

type Node = String

run :: IO ()
run = do
    ls <- readFile "src/day12.txt" <&> lines
    let graph = foldl parseEdge Map.empty ls
    let paths = dfs (Set.singleton "start") graph ["start"]
    putStrLn "Starting"
    putStrLn ("Part 1: " ++ show (length paths))
    let paths' = dfs' (Set.singleton "start") False graph ["start"]
    putStrLn ("Part 2: " ++ show (Set.size $ Set.fromList paths'))
    print "Done"

dfs' :: Set.Set Node -> Bool -> Map.Map Node [Node] -> [Node] -> [[Node]]
dfs' !seen !visitedSmallTwice !m [] = error "Empty list of paths"
dfs' seen visitedSmallTwice m ("end":xs) = [xs]
dfs' seen visitedSmallTwice m (x:xs) = do
    let newseen = if isUpper $ head x then seen else Set.insert x seen
    let simple = concat [dfs' newseen visitedSmallTwice m (v:x:xs)|v <-m!x, not $ Set.member v seen]
    if visitedSmallTwice then
        simple
    else 
        simple ++ concat [dfs' newseen True m (v:x:xs)|v <-m!x, v /= "start"]

dfs :: Set.Set Node -> Map.Map Node [Node] -> [Node] -> [[Node]]
dfs !seen !m [] = error "Empty list of paths"
dfs seen m ("end":xs) = [xs]
dfs seen m (x:xs) = do
    let newseen = if isUpper $ head x then seen else Set.insert x seen
    concat [dfs newseen m (v:x:xs)|v <-m!x, not $ Set.member v seen]

parseEdge :: Map.Map Node [Node] -> Node -> Map.Map Node [Node]
parseEdge m edge = Map.alter (alterf a) b (Map.alter (alterf b) a m)
    where
        [a,b] = splitOn "-" edge
        alterf z x = case x of
            Nothing -> Just [z]
            Just y -> Just (z:y)