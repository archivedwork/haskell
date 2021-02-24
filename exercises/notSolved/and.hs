module And where 
    -- and'[True, True, True, True]
    -- and'[] 
    -- not (and'[True, False, True, True])
    -- not (and'[False, False, False])

    and1 [] = True 
    and1 (x:xs)
        | x == True   = True 
        | x == False  = False
        | otherwise  = and1 xs