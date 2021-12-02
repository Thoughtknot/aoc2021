module Main where
import qualified Day1
import qualified Day2
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let day = read (head args) :: Int
    print "Hello, Haskell!"
    case day of
        1 -> Day1.run
        2 -> Day2.run
        _ -> error $ "Unknown day: " ++ show day
