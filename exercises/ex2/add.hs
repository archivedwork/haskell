module Add where
    -- add (1, 2) (1, 2) == (2, 2)
    -- add (4, 3) (6, 5) == (38, 15)

    add (x, y) (a,b)
        | x == a && y == b           = (x+a, y)
        | otherwise                 =  ((x*b+y*a), y*b)