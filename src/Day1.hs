module Day1 where

type Window = (Int, Int, Int)

run :: IO ()
run = do
    ls <- readFile "src/day1.txt"
    let a = map read (lines ls)
    print $ show $ countIncreases a (-1) 0
    let (a1, b) = (head a, tail a)
    let (a2, c) = (head b, tail b)
    let (a3, d) = (head c, tail c)
    let window = (a1, a2, a3)
    print $ show $ countIncreasesWindow d window 0
    print "Done" 

countIncreases :: [Int] -> Int -> Int -> Int
countIncreases [] _ n = n
countIncreases (x:xs) (-1) n = countIncreases xs x n
countIncreases (x:xs) prev n = countIncreases xs x new
    where
        new = if x > prev then n + 1 else n
        
countIncreasesWindow :: [Int] -> Window -> Int -> Int
countIncreasesWindow [] _ n = n
countIncreasesWindow (x:xs) (a,b,c) n = countIncreasesWindow xs newWindow new
    where
        newWindow = (b,c,x)
        val (a,b,c) = a + b + c
        new = if val newWindow > val (a,b,c) then n + 1 else n