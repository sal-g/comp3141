{-
 - Word Frequencies
 - Week one example
 - https://hoogle.haskell.org
 -}
import Data.Char (toLower)
import Data.List (sortBy, group, sort)

stringToWords :: String -> [String]
stringToWords = words

toLowerCase :: [String] -> [String]
toLowerCase = map (map toLower)

sortWords :: [String] -> [String]
sortWords = sort

-- Typedef in C
type Run = (String, Int)

countWordRuns :: [String] -> [Run]
countWordRuns = toRuns . groupCommonWords

-- Helper Function for countWordRuns
groupCommonWords :: [String] -> [[String]]
groupCommonWords = group

-- Helper Function for countWordRuns
toRuns :: [[String]] -> [Run]
toRuns = map (\list -> (head list, length list))

sortByRuns :: [Run] -> [Run]
sortByRuns = sortBy (\(w1,l1) (w2,l2) -> compare l2 l1)
-- sortByRuns = reverse . sortBy (\(w1,l1) (w2,l2) -> compare l1 l2)

firstNRuns :: Int -> [Run] -> [Run]
firstNRuns n = take n 

generateReport :: [Run] -> String
generateReport = unlines . map (\(w,l) -> w ++ ":" ++ show l)

-- commonWords :: Int -> String -> String
-- commonWords n s 
-- = (generateReport 
-- 	(firstNRuns n
-- 		(sortByRuns 
-- 			(countWordRuns 
-- 				(sortWords 
-- 	  				(toLowerCase 
-- 	  					(stringToWords s)))))))

-- commonWords :: Int -> String -> String
-- commonWords n s = ( generateReport
-- 				  . firstNRuns n
-- 				  . sortByRuns
-- 				  . countWordRuns
-- 				  . sortWords
-- 				  . toLowerCase
-- 				  . stringToWords) s

commonWords :: Int -> String -> String
commonWords n = generateReport
			  . firstNRuns n
			  . sortByRuns
			  . countWordRuns
			  . sortWords
			  . toLowerCase
			  . stringToWords