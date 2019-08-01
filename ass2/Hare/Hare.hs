{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  -- Char :: Matches a single character from the list
  Char :: [Char] -> RE Char
  -- Seq :: Matches the two expressions in sequence
  Seq :: RE a -> RE b -> RE (a, b)
  -- Choose :: Matches either the first, or the second
  Choose :: RE a -> RE a -> RE a
  -- Star :: Matches the expression zero or more times
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

-- Results type describes the string that was matched by each part of the regular expression
data Results = None
             | Character Char
             | Tuple Results Results
             | Repetition [Results]

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure () -- pure None -- Seeing as the Empty expression matches any string, Helen simply returns None
match Fail = failure -- Fail matches no strings at all, Helen always calls failure
match (Char cs) = do -- Helen uses the built-in function guard, which will fail if the given condition evaluates to false
  x <- readCharacter
  guard (elem x cs)
  -- guard (x `elem` cs)
  -- pure (Character x)
  pure x
match (Seq a b) = do -- Helen makes use of the Monad instance, first matching on the first expression a, then the second b
  ra <- match a
  rb <- match b
  -- pure (Tuple ra rb)
  pure (ra, rb)
{-
 - Because there is no dependencies between ra and rb, this could also have been defined
 - using the Applicative instance, as follows: 
 - match (Seq a b) = Tuple <$> match a <*> match b
 -}
match (Choose a b) = match a <|> match b -- Helen makes use of the Alternative (<|>) method
-- match (Star a) = addFront <$> match a <*> match (Star a) <|> pure (Repetition []) where
--   addFront x (Repetition xs) = Repetition (x : xs)
--   addFront _ _ = error "(should be) impossible!"
match (Star a) = addFront <$> match a <*> match (Star a) <|> pure [] where
  addFront x xs = (x : xs)
-- match re = error "'match' unimplemented"
match (Action a b) = fmap a (match b)

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(a, b) -> a : b) (Seq x xs)
  -- error "'cons' unimplemented"

string :: String -> RE String
string [] = Action (\x -> []) Empty 
string (x:xs) = cons (Char [x]) (string xs)
  -- error "'string' unimplemented"

rpt :: Int -> RE a -> RE [a]
rpt 0 re = Action(\r -> []) Empty
rpt n re = cons (re) (rpt (n - 1) re)
  -- error "'rpt' unimplemented"

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = choose (fmap (\n -> rpt n re) [y, y - 1 .. x])
  -- error "'rptRange' unimplemented"

option :: RE a -> RE (Maybe a)
option re = Choose (Action(\r -> Nothing) Empty) (Action(\r -> Just r) re)
  -- error "'option' unimplemented"

plus :: RE a -> RE [a]
plus re = Action (\(a, b) -> a : b) (Seq re (Star re))
  -- error "'plus' unimplemented"

choose :: [RE a] -> RE a
choose [] =  Fail
choose (x:xs) = Choose x (choose xs)
  -- error "'choose' unimplemented"

