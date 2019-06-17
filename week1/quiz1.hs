-- Quiz 1

-- q6 $ is infix 
q6 = map ($ 5) [(-),(+),(*)]
-- >> map ($1) q6

-- q7
-- import Data.Char (ord, chr)

-- let increment x = 1 + x;
-- \xs -> map chr (map increment (map ord xs));
-- >> (\xs -> map chr (map increment (map ord xs))) <list>

-- q8
-- q8 = foldr (&&) True . map (>= 0);
-- q8a = and . map (>= 0);
-- q8b = all (>= 0);
-- q8c = any (>= 0);
-- q8d = foldr (\a b -> a >= 0 && b) True;
-- q8e = foldl (\a b -> a && b > 0) True;