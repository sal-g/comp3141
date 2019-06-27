module Ex02 where
import Test.QuickCheck
import Data.List
-- implement the following functions, which meet some (but not all!) of the 
-- properties of a correct sorting function

-- prop2 & 4, but not prop1 & 3 & 5
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs
	-- error "'dodgySort1' unimplemented"


-- prop1 & 2 & 3, but not prop4 & 5
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = insertionSort $ 0 : xs
	-- error "'dodgySort2' unimplemented"


-- prop1 & 3 & 4, but not prop2 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = insertionSort (swapFirstElem xs) where
swapFirstElem :: [Int] -> [Int]
swapFirstElem [] = []
swapFirstElem xs = 0 : (delete (minimum xs) xs)
	-- error "'dodgySort3' unimplemented"


-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = insertionSort list 
 where
  list = case duplicates (insertionSort xs) of
      Just duplicate -> (1 : (delete duplicate xs))
      otherwise      -> xs 
    -- error "'dodgySort3' unimplemented"
duplicates :: [Int] -> Maybe Int
duplicates [] = Nothing
duplicates (x:xs) = if count x (x:xs) > 1 then Just x else duplicates xs

count :: Int -> [Int] -> Int
count x [] = 0
count x (y:ys) = if x /= y then count x ys else 1 + count x ys


-- Properties of sorting function
{-
 - prop 1 -> sort function takes in a reverse list
 - prop 2 -> sort function should all elements of list
 - prop 3 -> sort function should sort the list in asc order
 - prop 4 -> sort function should return the same length
 - prop 5 -> insertion sort
 -}    
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where 
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True
    
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool    
sortProp4 sortFn xs = length xs == length (sortFn xs)

sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs 
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where 
    insertSorted x [] = [x]
    insertSorted x (y : ys) 
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

