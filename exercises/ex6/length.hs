module Length where
    -- length'[]      == 0
    -- length'[5]     == 1
    -- length'[8,0,3] == 3


    length1 []     = 0
    length1 [x]    = 1
    length1 (x:xs) = myLen 1 (length((x:xs)))


    range x y
        | y > x =  x : range (x+1) (y)
        | otherwise = []


    myLen x y = length(range x y)