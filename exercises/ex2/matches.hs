module Matches where
    -- matches (2, 4) (4, 6)
    -- matches (4, 2) (4, 6)
    -- matches (6, 2) (4, 6)
    -- not (matches (2, 8) (4, 6))

    matches (x,y) (a,b)
        | x == a  || y == a || x == b   =  True 
        | otherwise                     = False 

        