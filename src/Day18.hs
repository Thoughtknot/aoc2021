module Day18 where
import Data.Functor
import Data.List.Split.Internals (splitOn)
import Debug.Trace
import Text.Parsec.String (Parser)
import Text.Parsec (digit, char, (<|>), parse)
import Data.Char (digitToInt)
import Data.IORef
import Control.Monad

type Marked = Bool

data SFNumber = Pair SFNumber SFNumber
    | Lit Int
    deriving Show

data ISFNumber = IPair (IORef ISFNumber) (IORef ISFNumber)
    | ILit Int

run :: IO ()
run = do
    ls <- readFile "src/day18.txt" <&> lines <&> map parseLine
    cla <- mapM constructSF ls >>= mapM newIORef
    result <- reduceAll (head cla) (tail cla)
    mag <- readIORef result >>= calculateMagnitude
    putStrLn $ "Part 1: " ++ show mag
    let ap = allPairs ls
    result' <- mapM reducePair ap
    putStrLn $ "Part 2: " ++ show (maximum result')
    putStrLn "Done"

reducePair :: (SFNumber, SFNumber) -> IO Int
reducePair (a,b) = do
    isf <- constructSF (Pair a b) >>= newIORef
    reduce isf
    readIORef isf >>= calculateMagnitude
    --print (c,a,b)

allPairs :: [SFNumber] -> [(SFNumber, SFNumber)]
allPairs ls = [(x, y) | x <- ls, y <- ls]

showIsf :: ISFNumber -> IO String
showIsf (ILit n) = return $! show n
showIsf (IPair l r) = do
    ll <- readIORef l >>= showIsf
    rr <- readIORef r >>= showIsf
    return $ "[" ++  ll ++ "," ++ rr ++ "]"

reduceAll :: IORef ISFNumber -> [IORef ISFNumber] -> IO (IORef ISFNumber)
reduceAll x [] = do
    reduce x
    return x
reduceAll x (h:xs) = do
    n <- newIORef $! IPair x h
    reduce n
    reduceAll n xs

reduce :: IORef ISFNumber -> IO ()
reduce risf = do
    isf <- readIORef risf
    --showIsf isf >>= putStrLn
    a <- explodeRef risf
    if a then
        reduce risf
    else do
        b <- splitRisf risf
        when b (reduce risf)

calculateMagnitude :: ISFNumber -> IO Int
calculateMagnitude (ILit x) = return x
calculateMagnitude (IPair l r) = do
    ll <- readIORef l >>= calculateMagnitude
    rr <- readIORef r >>= calculateMagnitude
    return $! 3*ll + 2*rr

splitRisf :: IORef ISFNumber -> IO Bool
splitRisf risf = do
    isf <- readIORef risf
    (a,_) <- splitIsf isf
    return a

splitIsf :: ISFNumber -> IO (Bool, Maybe Int)
splitIsf (ILit x) = if x >= 10 then return (True, Just x) else return (False, Nothing)
splitIsf (IPair l r) = do
    (nl, m) <- readIORef l >>= splitIsf
    if nl then
        case m of
            Just x -> do
                il <- newIORef $ ILit (div x 2)
                ir <- newIORef $ ILit (div x 2 + rem x 2)
                writeIORef l (IPair il ir)
                return (True, Nothing)
            Nothing -> return (True, Nothing)
    else do
        (nr, m) <- readIORef r >>= splitIsf
        case (nr, m) of
            (True, Just x) -> do
                il <- newIORef $ ILit (div x 2)
                ir <- newIORef $ ILit (div x 2 + rem x 2)
                writeIORef r (IPair il ir)
                return (True, Nothing)
            (True, Nothing) -> return (True, Nothing)
            (False, _) -> return (False, Nothing)

explodeRef :: IORef ISFNumber -> IO Bool
explodeRef risf = do
    isf <- readIORef risf
    (a,b) <- explode isf 0
    return a

explode :: ISFNumber -> Int -> IO (Bool, (Maybe Int, Maybe Int))
explode (ILit _) i = return (False, (Nothing, Nothing))
explode (IPair l r) 4 = do
    ll <- readIORef l
    rr <- readIORef r
    case (ll, rr) of
        (ILit a, ILit b) -> return (True, (Just a, Just b))
        (_, _) -> return (False, (Nothing, Nothing))
explode (IPair l r) i = do
    ll <- readIORef l
    rr <- readIORef r
    (lt, p) <- explode ll (i + 1)
    if lt then do
        --rrs <- readIORef r >>= showIsf
        --print ("Lt:", lt, p, rrs)
        case (rr,p) of
            (ILit n, (Just a, Just b)) -> do
                writeIORef l (ILit 0)
                writeIORef r (ILit (n + b))
                return (True, (Just a, Nothing))
            (ILit n, (Nothing, Just b)) -> do
                writeIORef r (ILit (n + b))
                return (True, (Nothing, Nothing))
            (rr@(IPair _ _), (Just a, Just b)) -> do
                writeIORef l (ILit 0)
                writeLeft rr b
                return (True, (Just a, Nothing))
            (rr@(IPair _ _), (Nothing, Just b)) -> do
                writeLeft rr b
                return (True, (Nothing, Nothing))
            _ -> return (True, p)
    else do
        (rt, rp) <- explode rr (i + 1)
        --lls <- readIORef l >>= showIsf
        --print ("Rt:", rt, rp, lls)
        if rt then
            case (ll,rp) of
                (ILit n, (Just a, Just b)) -> do
                    writeIORef l (ILit (n + a))
                    writeIORef r (ILit 0)
                    return (True, (Nothing, Just b))
                (ILit n, (Just a, Nothing)) -> do
                    writeIORef l (ILit (n + a))
                    return (True, (Nothing, Nothing))
                (ll@(IPair _ _), (Just a, Just b)) -> do
                    writeIORef r (ILit 0)
                    writeRight ll b
                    return (True, (Nothing, Just b))
                (ll@(IPair _ _), (Just b, Nothing)) -> do
                    writeRight ll b
                    return (True, (Nothing, Nothing))
                _ -> return (True, rp)
        else
           return (False, (Nothing, Nothing))

writeRight :: ISFNumber -> Int -> IO Bool
writeRight (ILit n) v = return False
writeRight (IPair l r) v = do
    rr <- readIORef r
    case rr of
      IPair irl irr -> do
          rl <- readIORef irl
          writeRight rr v
      ILit n -> do
          writeIORef r (ILit (n + v))
          return True

writeLeft :: ISFNumber -> Int -> IO Bool
writeLeft (ILit n) v = return False
writeLeft (IPair l r) v = do
    ll <- readIORef l
    case ll of
      IPair irl irr -> do
          rl <- readIORef irl
          writeLeft ll v
      ILit n -> do
          writeIORef l (ILit (n + v))
          return True

constructSF :: SFNumber -> IO ISFNumber
constructSF (Lit x) = return $! ILit x
constructSF (Pair l r) = do
    ll <- constructSF l >>= newIORef
    rr <- constructSF r >>= newIORef
    return (IPair ll rr)

parseLine :: String -> SFNumber
parseLine l =
    case parse parsePair "snailfish" l of
      Left pe -> error (show pe)
      Right sn -> sn

parseSFNumber :: Parser SFNumber
parseSFNumber = parseLiteral <|> parsePair

parseLiteral :: Parser SFNumber
parseLiteral = Lit . digitToInt <$> digit

parsePair :: Parser SFNumber
parsePair = do
    char '['
    l <- parseSFNumber
    char ','
    r <- parseSFNumber
    char ']'
    return $! Pair l r