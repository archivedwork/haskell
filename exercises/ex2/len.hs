module Len where 
    

    -- len (3, 4)  == 5.0
    -- len (4, 3)  == 5.0
    -- len (5, 12) == 13.0


    len (x, y)
        | length [0..x] > length [0..y]  = length [0..x]
        | otherwise                       = length[0..y]