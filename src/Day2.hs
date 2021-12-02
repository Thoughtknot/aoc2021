module Day2 where

data Direction = Forward | Down | Up
type Instruction = (Direction, Int)
type Position = (Int, Int)
type Position' = (Int, Int, Int)
run :: IO ()
run = do
    ls <- readFile "src/day2.txt"
    let l = lines ls
    let instructions = map parseInstruction l
    let (x,y) = foldl handleInstruction (0,0) instructions
    print ("Part 1: " ++ show (x*y))
    let (x',y',a') = foldl handleInstruction' (0,0,0) instructions
    print ("Part 2: " ++ show (x'*y'))
    print "Done"

handleInstruction' :: Position' -> Instruction -> Position'
handleInstruction' p x =
    case x of
        (Forward, v) -> move p (v, aim*v, 0)
        (Up, v) -> move p (0, 0, -v)
        (Down, v) -> move p (0, 0, v)
    where
        (_, _, aim) = p
        move (a,b,c) (x,y,z) = (a+x,b+y,c+z)

handleInstruction :: Position -> Instruction -> Position
handleInstruction p x =
    case x of
        (Forward, v) -> move p (v, 0)
        (Up, v) -> move p (0, -v)
        (Down, v) -> move p (0, v)
    where
        move (a,b) (c,d) = (a+c,b+d)

parseInstruction :: String -> Instruction
parseInstruction line
    | dir == "forward" = (Forward, mv)
    | dir == "down" = (Down, mv)
    | dir == "up" = (Up, mv)
    | otherwise = error "Wrong"
        where
            [dir, x] = words line
            mv = read x :: Int

