module Triangle where
    -- Check whether a triangle with three given sides can be drawn
    -- triangleSides 2 1 2
    -- not (triangleSides 3 4 1)


    triangle x y z
        | x == z        = True 
        | otherwise     = False