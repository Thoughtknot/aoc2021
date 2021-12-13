module Day13 where
import Data.Functor
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Map ((!?))

type Pos = (Int, Int)
data Fold = Horizontal Int | Vertical Int deriving Show

run :: IO ()
run = do
    ls <- readFile "src/day13.txt" <&> lines
    let (m, remls) = parseLines ls Map.empty
    let result = fold (head remls) m
    putStrLn ("Part 1: " ++ show (length result))
    let result = foldl (flip fold) m remls
    putStrLn "Part 2: "
    printResult result
    print "Done"

printResult :: Map.Map Pos Bool -> IO ()
printResult m = do
    let ls = map fst $ Map.toList m
    let minY = minimum $ map snd ls
    let maxY = maximum $ map snd ls
    let minX = minimum $ map fst ls
    let maxX = maximum $ map fst ls
    let showDot x y =case m !? (x, y) of
          Nothing -> " "
          Just True -> "#"
          Just False -> " "
    let constructLine y = concat[showDot x y | x<-[minX-1..maxX+1]]
    let lines = [constructLine y | y <-[minY-1..maxY+1]]
    mapM_ print lines

fold :: Fold -> Map.Map Pos Bool -> Map.Map Pos Bool
fold (Vertical v) = Map.mapKeys (\(x,y) -> if x < v then (x,y) else (v-(x-v),y))
fold (Horizontal v) = Map.mapKeys (\(x,y) -> if y < v then (x,y) else (x,v-(y-v)))

parseLines :: [String] -> Map.Map Pos Bool -> (Map.Map Pos Bool, [Fold])
parseLines [] m = error "no fold instructions"
parseLines ("":xs) m = (m, reverse $ foldl (\ f x -> parseFold x : f) [] xs)
parseLines (x:xs) m = parseLines xs newm
    where
        [a,b] = splitOn "," x
        newm = Map.insert (read a, read b) True m

parseFold :: String -> Fold
parseFold line
    | a == "y" = Horizontal (read b)
    | a == "x" = Vertical (read b)
    | otherwise = error $ "Unknown fold: " ++ line
    where
        [a,b] = splitOn "=" . last $ words line
