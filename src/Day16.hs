module Day16 where
import Data.Functor
import Data.List.Split

import qualified Data.Vector.Storable.Mutable as IOVector
import Data.List (foldl')
import Data.Char
import Debug.Trace

type Version = Int
type Bits = [Char]

data Packet = Literal Version Int
    | Sum Version [Packet]
    | Product Version [Packet]
    | Min Version [Packet]
    | Max Version [Packet]
    | Gt Version [Packet]
    | Lt Version [Packet]
    | Eq Version [Packet]
    deriving Show

run :: IO ()
run = do
    ls <- readFile "src/day16.txt" <&> map toBinary <&> concat
    let (r,_) = parsePacket ls
    let result = sumVersionNumbers r
    print ("Part 1: " ++ show result)
    let result' = eval r
    print ("Part 2: " ++ show result')
    print "Done"

eval :: Packet -> Int
eval (Literal v x) = x
eval (Sum v ls) = sum (map eval ls)
eval (Product v ls) = product (map eval ls)
eval (Max v ls) = maximum (map eval ls)
eval (Min v ls) = minimum (map eval ls)
eval (Gt v [a, b]) = if eval a > eval b then 1 else 0
eval (Lt v [a, b]) = if eval a < eval b then 1 else 0
eval (Eq v [a, b]) = if eval a == eval b then 1 else 0
eval _ = error "Unknown packet type"

sumVersionNumbers :: Packet -> Int
sumVersionNumbers (Literal v t) = v
sumVersionNumbers (Sum v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Product v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Min v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Max v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Gt v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Lt v ls) = v + sum (map sumVersionNumbers ls)
sumVersionNumbers (Eq v ls) = v + sum (map sumVersionNumbers ls)

parsePacket :: Bits -> (Packet, Bits)
parsePacket ls
    | tp == "100" = parseLiteral ver r
    | tp == "000" = (Sum (toInt ver) body, bits)
    | tp == "001" = (Product (toInt ver) body, bits)
    | tp == "010" = (Min (toInt ver) body, bits)
    | tp == "011" = (Max (toInt ver) body, bits)
    | tp == "101" = (Gt (toInt ver) body, bits)
    | tp == "110" = (Lt (toInt ver) body, bits)
    | tp == "111" = (Eq (toInt ver) body, bits)
    | otherwise = error $ "Unknown type " ++ tp
    where
        (vt, r) = splitAt 6 ls
        (ver, tp) = splitAt 3 vt
        (body, bits) = parseOperator r

parseOperator :: Bits -> ([Packet], Bits)
parseOperator ('0':r) = (parseOperatorBody (take len b) [], drop len b)
    where
        (a, b) = splitAt 15 r
        len = toInt a
parseOperator ('1':r) = (ls, t)
    where
        (a, b) = splitAt 11 r
        (ls, t) = parseOperatorBodyN (toInt a) b []
parseOperator x = trace ("Operator empty: " ++ x) error "can't parse empty list"

parseOperatorBodyN :: Int -> Bits -> [Packet] -> ([Packet], Bits)
parseOperatorBodyN 0 b p = (reverse p, b)
parseOperatorBodyN n b p = parseOperatorBodyN (n-1) newb (np:p)
    where
        (np, newb) = parsePacket b

parseOperatorBody :: Bits -> [Packet] -> [Packet]
parseOperatorBody [] p = reverse p
parseOperatorBody ls p = let (n,r) = parsePacket ls in parseOperatorBody r (n:p)

parseLiteral :: Bits -> Bits -> (Packet, Bits)
parseLiteral ver r = (Literal (toInt ver) l, c)
    where
        (l,c) = parseLiteralValue r []

parseLiteralValue :: Bits -> Bits -> (Int, Bits)
parseLiteralValue ('0':xs) i =  let (as, a) = splitAt 4 xs in (toInt (i ++ as), a)
parseLiteralValue ('1':xs) i = let (as, a) = splitAt 4 xs in parseLiteralValue a (i ++ as)
parseLiteralValue _ i = error "empty list"

toInt :: Bits -> Int
toInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

toBinary :: Char -> Bits
toBinary '0' = "0000"
toBinary '1' = "0001"
toBinary '2' = "0010"
toBinary '3' = "0011"
toBinary '4' = "0100"
toBinary '5' = "0101"
toBinary '6' = "0110"
toBinary '7' = "0111"
toBinary '8' = "1000"
toBinary '9' = "1001"
toBinary 'A' = "1010"
toBinary 'B' = "1011"
toBinary 'C' = "1100"
toBinary 'D' = "1101"
toBinary 'E' = "1110"
toBinary 'F' = "1111"
toBinary _ = error "Not valid hex"