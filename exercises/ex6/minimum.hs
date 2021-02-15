module Menimum where
    -- minimum'[0] == 0
    -- minimum'[9, 3, 4, 1, 10] == 1
    minimum1 [] = 0
    minimum1[x] = x
    minimum1 (x:y:xs)
        | x  < y       = minimum1 (x:xs)
        | otherwise   =  minimum1 (y:xs)



-- with helper function 

    min1 (x:xs) = minHelper x xs


    minHelper x [] = x
    minHelper x (y:ys)
        | x < y     = minHelper x ys
        | otherwise = minHelper y ys
