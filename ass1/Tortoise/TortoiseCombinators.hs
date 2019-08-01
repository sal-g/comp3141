module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen (Move distance i1) i2 = Move distance (andThen i1 i2)
andThen (Turn angle i1) i2 = Turn angle (andThen i1 i2)
andThen (SetStyle lineStyle i1) i2 = SetStyle lineStyle (andThen i1 i2)
andThen (SetColour colour i1) i2 = SetColour colour (andThen i1 i2)
andThen (PenDown i1) i2 = PenDown (andThen i1 i2)
andThen (PenUp i1) i2 = PenUp (andThen i1 i2)
andThen Stop i1 = i1
-- andThen i1 Stop = i1 
	-- error "'andThen' unimplemented"
{-
 - The expression loop 0 i should be equivalent to Stop as should any negative n
 - Any positive n should produce the composition of n copies of i
 -}
loop :: Int -> Instructions -> Instructions
loop n i = if n <= 0 then Stop 
           else andThen i (loop (n-1) i)
	-- error "'loop' unimplemented"

invisibly :: Instructions -> Instructions
invisibly i1 = PenUp (inv_combinators i1 False) 
	where 
	inv_combinators :: Instructions -> Bool -> Instructions
	inv_combinators (Move distance i1) penState = Move distance (inv_combinators i1 penState)
	inv_combinators (Turn angle i1) penState = Turn angle (inv_combinators i1 penState)
	inv_combinators (SetStyle lineStyle i1) penState = SetStyle lineStyle (inv_combinators i1 penState)
	inv_combinators (SetColour colour i1) penState = SetColour colour (inv_combinators i1 penState)
	inv_combinators (PenDown i1) penState = inv_combinators i1 False -- pen is not up
	inv_combinators (PenUp i1) penState = inv_combinators i1 True -- pen is up
	inv_combinators stop penState = if penState == True then Stop
		                              else PenDown Stop	                        
	-- error "'retrace' unimplemented"
		                              
retrace :: Instructions -> Instructions
retrace i1 = re_combinators i1 i2 lineStyle colour penState 
	where
	i2 :: Instructions
	i2 = Stop

	lineStyle :: LineStyle
	lineStyle = Solid 1 -- tortoise state (start)

	colour :: Colour
	colour = white -- tortoise state (start)

	penState :: Bool
	penState = False

	re_combinators :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
	re_combinators (Move distance i1) i2 lineStyle colour penState = re_combinators i1 (Move (-distance) i2) lineStyle colour penState
	re_combinators (Turn angle i1) i2 lineStyle colour penState = re_combinators i1 (Turn (-angle) i2) lineStyle colour penState
	re_combinators (SetStyle newLineStyle i1) i2 lineStyle colour penState = re_combinators i1 (SetStyle lineStyle i2) newLineStyle colour penState
	re_combinators (SetColour newColour i1) i2 lineStyle colour penState = re_combinators i1 (SetColour colour i2) lineStyle newColour penState                        
	re_combinators (PenDown i1) i2 lineStyle colour penState = if penState == True then re_combinators i1 (PenUp i2) lineStyle colour False
		                                                       else re_combinators i1 (PenDown i2) lineStyle colour False
	re_combinators (PenUp i1) i2 lineStyle colour penState = if penState == True then re_combinators i1 (PenUp i2) lineStyle colour True
		                                                       else re_combinators i1 (PenDown i2) lineStyle colour True                               
	re_combinators Stop i2 _ _ _ = i2
	-- re_combinators i1 i2 _ _ _ = i1
	-- error "'retrace' unimplemented"

overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (i:is) = i `andThen` (invisibly (retrace i)) `andThen` (overlay is)
	-- error "'overlay' unimplemented"
