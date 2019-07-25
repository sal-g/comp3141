{-# LANGUAGE FlexibleContexts #-}
module Ex05 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck

capitalise :: FilePath -> FilePath -> IO ()
capitalise i o = do
  str <- readFile i
  writeFile o (map toUpper str)
  -- error "'capitalise' unimplemented"

sumFile :: IO ()
sumFile = do
  [i, o] <- getArgs
  str <- readFile i
  -- let line = lines str
  writeFile o $ show $ sum $ map read $ lines str
  -- error "'sumFile' unimplemented"

data Player m = Player { guess :: m Int
                       , wrong :: Answer -> m ()
                       }
data Answer = Lower | Higher

guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
  where
   go 0 = pure False
   go n = do
     x' <- guess p
     case compare x x' of
       LT -> wrong p Lower  >> go (n-1)
       GT -> wrong p Higher >> go (n-1)
       EQ -> pure True

human :: Player IO
human = Player { guess = guess, wrong = wrong }
  where
    guess = do
      putStrLn "Enter a number (1-100):"
      x <- getLine
      case readMaybe x of
        Nothing -> guess
        Just i  -> pure i

    wrong Lower  = putStrLn "Lower!"
    wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1,100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")


midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo

-- State is a monad 
-- put sets the state
-- get leaves state unchanged and set result to state
ai :: Player (State (Int,Int))
ai = Player { guess = guess, wrong = wrong}
  where
    -- (lo, hi) <- get
    guess = do
      (lo, hi) <- get
      pure (midpoint lo hi)
      
    wrong Lower  = do
      (lo, hi) <- get
      put (lo, midpoint lo (hi - 1))

    wrong Higher = do
      (lo, hi) <- get
      put (midpoint lo (hi + 1), hi)
  -- error "'ai' unimplemented"

prop_basic (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x n ai) (1,n)

prop_optimality (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x (bound n) ai) (1,n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1


