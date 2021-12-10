module Day10 where
import Data.Functor
import Debug.Trace
import Data.List (sort)
import qualified Data.Sequence as Seq

run :: IO ()
run = do
    ls <- readFile "src/day10.txt" <&> lines
    let getVal x
            | x == ')' = 3
            | x == ']' = 57
            | x == '}' = 1197
            | x == '>' = 25137
            | otherwise = error "Unknown paren"
    let result = map (`parseLine` []) ls
    let fails = filter (\(a,b,c) -> not c) result
    putStrLn ("Part 1: " ++ show (sum $ map (\(a,b,c) -> getVal $ head b) fails))
    let notfails = Seq.fromList $ sort $ map (\(a,b,c) -> getScore 0 $ map inverseBracket a) $ filter (\(a,b,c) -> c) result
    let result' = Seq.index notfails $ div (Seq.length notfails) 2 
    putStrLn ("Part 2: " ++ show result')
    print "Done"

getScore :: Int -> [Char] -> Int
getScore v [] = v
getScore v (h:xs) = getScore (v*5 + getVal' h) xs
    where
        getVal' x
            | x == ')' = 1
            | x == ']' = 2
            | x == '}' = 3
            | x == '>' = 4
            | otherwise = error "Unknown paren"

parseLine :: [Char] -> [Char] -> ([Char], [Char], Bool)
parseLine [] c = (c, [], True)
parseLine (x:xs) c = do
    if isOpen x then
        parseLine xs (x:c)
    else
        if inverseBracket (head c) == x then
            parseLine xs (tail c)
        else
            (c, x:xs, False)
    where
        isOpen c = c `elem` "{([<"

inverseBracket :: Char -> Char
inverseBracket '[' = ']'
inverseBracket '(' = ')'
inverseBracket '<' = '>'
inverseBracket '{' = '}'
inverseBracket _ = error "Unknown char "