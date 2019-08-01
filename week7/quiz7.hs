import Control.Monad.State
import Data.Char

-- while :: (t0 a0 -> Bool) -> m0 () -> State String ()
-- leftPad :: Int -> State String ()
-- leftPad l = while ((< l) . length) $ do
--               str <- get
--               put (' ':str)

a = do x <- getLine
       putStrLn (filter isDigit x)
       a

b = getLine >>= putStrLn . filter isDigit >> b

c = getLine >>= \x -> putStrLn (filter isDigit x) >>= \_ -> c

-- d = (getLine >>= \x -> putStrLn (filter isDigit x)) >>= d

-- e = do getLine >>= \x -> putStrLn . filter isDigit; e

-- f = do x <- getLine; putStrLn . filter isDigit; f

g = do x <- getLine; putStrLn . filter isDigit $ x; g

h = do getLine >>= \x -> putStrLn (filter isDigit x); h

-- matching1 xs = snd (runState (go xs) 0) == 0
--   where
--     go [] = pure True
--     go (x:xs) | x == '('  = modify (+1) >> go xs
--               | x == ')'  = modify (-1) >> go xs
--               | otherwise = go xs

matching2 xs = snd (runState (go xs) 0) == 0
  where
    go [] = pure True
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 

matching3 xs = fst (runState (go xs) 0)
  where
    go [] = pure True
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 

-- matching4 xs = let (b,n) = runState (go xs) 0
--                in b && n == 0
--   where
--     go [] = pure True
--     go (x:xs) | x == '('  = modify (+1) >> go xs
--               | x == ')'  = modify (-1) >> go xs
--               | otherwise = go xs 

matching5 xs = fst (runState (go xs) 0)
  where
    go [] = get >>= pure . (== 0)
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 

adder :: Int -> Int
adder x = 