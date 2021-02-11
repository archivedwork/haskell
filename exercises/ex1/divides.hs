module Divides where 
-- Define a function which checks whether an integer divides another.
--  2`divides`4 
-- not (4`divides`2)
-- 3`divides`9


divides x y = x `mod` y /= 0



-- other way with guards
divides1 x y 
    | (x `mod` y /= 0)    = True 
    | otherwise           = False