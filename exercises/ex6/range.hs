module Range where 
    -- range 5 9 == [5, 6, 7, 8, 9]
    -- range 5 5 == [5]
    -- range 0 3 == [0, 1, 2, 3]

    
    range x y
        | y > x    =  x : range (x+1) (y)
        | x == y   = [x]
        | otherwise = []    -- base case




    --  range in decreasing sequence
    range2 x y 
        | y > x = range x y 
        | y < x = x : range (x) (y+1)
        | y == x    = [x]
        | otherwise = []