module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
-- words :: String -> [String]
-- function composition
tokenise = mapM parseToken . words
  -- error "'tokenise' unimplemented"


newtype Calc a = C ([Int] -> Maybe ([Int], a))

-- Stack
{-
 - do nothing if empty stack
 -}
pop :: Calc Int
pop = C remove where
  --remove :: [Int] -> Maybe ([Int], Int)
  remove :: [a] -> Maybe ([a], a)
  remove [] = Nothing
  remove (x:xs) = Just (xs, x)
  -- error "'pop' unimplemented"

push :: Int -> Calc ()
push i = C (\xs -> Just ((i:xs),())) -- Expected type: Maybe ([Int], ())
  -- error "'push' unimplemented"

instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate ts = do
  evaluates ts 
  pop where
    evaluates [] = pure 1
    evaluates (Number t:ts) = do
      push t
      evaluates ts
    evaluates (Operator t:ts) = do
      x <- pop
      y <- pop
      push (x `t` y)
      evaluates ts
  -- error "'evaluate' unimplemented"


result :: Maybe (Maybe ([Int], Int)) -> Maybe Int
result (Just (Just(_, x))) = Just x
result _ = Nothing

unwrapCal :: Calc Int -> Maybe ([Int], Int)
unwrapCal c = unwrapCalc c []
  where unwrapCalc (C a) = a

calculate :: String -> Maybe Int
-- calculate s = result $ pure <$> evaluate <$> tokenise s
calculate s = result $ unwrapCal <$> evaluate <$> tokenise s
  -- error "'calculate' unimplemented"

