import Data.Char(toUpper,isAlpha,ord)
import Data.List(sort, nub)

-- Question 1 [1,2,3,5,7]
rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))

-- Question 2 [1,2,3,4,6]
merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

-- Question 3 [1,4]
toBinary :: Int -> String
toBinary 0 = ""
toBinary n = let (d,r) = n `divMod` 2
              in toBinary d 
                   ++ if r == 0 then "0"
                                else "1"

fromBinary :: String -> Int
fromBinary = fst . foldr eachChar (0,1)
  where
    eachChar '1' (sum, m) = (sum + m, m*2)
    eachChar _   (sum, m) = (sum    , m*2)

-- Question 4 [1,2,3,6]
dedup :: (Eq a) => [a] -> [a]
dedup (x:y:xs) | x == y = dedup (y:xs)
               | otherwise = x : dedup (y:xs)
dedup xs = xs

sorted :: (Ord a) => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted xs = True

-- Question 5 [4]
tuple1 :: Int -> (Int, Int)
tuple1 x = (x,x)

tuple2 :: Int -> (Int, Int)
tuple2 x = (x,x)

foo :: [a] -> (a -> b) -> [b]
foo [] f = []
foo (x:xs) f = f x : foo xs f -- Option 4
-- foo [] f = []
-- foo (x:xs) f = foo xs f -- Option 5
-- foo [] f = []
-- foo (x:xs) f = x : foo xs f -- Option 3
-- foo xs f = xs -- Option 2
-- foo xs f = [] -- Option 1
-- foo = undefined 

prop_1 :: [Int] -> Bool
prop_1 xs = foo xs id == xs 

prop_2 :: [Int] -> (Int -> Int) -> (Int -> Int) -> Bool
prop_2 xs f g = foo (foo xs f) g == foo xs (g . f)

-- Question 6 [2, 4]
bar :: [Int] -> [Int]
bar = id -- Option 4 id is identity function
-- bar xs = go xs []
--   where go []     acc = acc
--         go (x:xs) acc = go xs (x:acc) -- Option 2
-- bar = undefined

prop_3 :: [Int] -> Bool
prop_3 xs = bar (bar xs) == xs

prop_4 :: [Int] -> Bool
prop_4 xs = length xs == length (bar xs)

prop_5 :: [Int] -> (Int -> Int) -> Bool
prop_5 xs f = bar (map f xs) == map f (bar xs)

-- Question 7 [1]
baz :: [Integer] -> Integer
baz = foldr (+) 0 -- Option 1
-- baz []     = 0
-- baz (x:xs) = 1 + baz xs -- Option 2
-- baz = undefined

prop_6 :: [Integer] -> [Integer] -> Bool
prop_6 xs ys = baz xs + baz ys == baz (xs ++ ys)

prop_7 :: [Integer] -> Bool
prop_7 xs = baz xs == baz (reverse xs) 

prop_8 :: Integer -> [Integer] -> Bool 
prop_8 x xs = baz (x:xs) - x == baz xs

-- Question 8 [3]
fun :: [Integer] -> [Integer]
fun []       = []
fun [x]      = []
fun (x:y:xs) = (y-x):fun (y:xs)

nuf :: [Integer] -> Integer -> [Integer]
nuf xs i = scanl (\v x -> v + x) i xs -- Option 3
-- nuf [] i = []
-- nuf (x:xs) i = (i + x) : nuf xs (i + x) -- Option 1
-- nuf [] i = [i]
-- nuf (x:xs) i = (i + x) : nuf xs (i + x) -- Option 2
-- nuf [] i = []
-- nuf (x:xs) i = (i + x) : nuf xs i -- Option 4
-- nuf xs i = i : scanl (\v x -> v + x) i xs -- Option 5
-- nuf = undefined

prop_9 :: [Integer] -> Integer -> Bool
prop_9 xs x = nuf (fun (x:xs)) x == (x:xs)

prop_10 :: [Integer] -> Integer -> Bool
prop_10 xs x = fun (nuf xs x) == xs