module Even where 

-- Define a function which checks whether its parameter is even. Function even already exists in Haskell, so lets call our function even'.
-- even'2 
-- not (even'3)
-- even'(-4)

even' x = (x `mod` 2 == 0) 
