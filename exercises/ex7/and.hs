-- and'[True, True, True, True]
-- and'[]
-- not (and'[True, False, True, True])
-- not (and'[False, False, False])

and' :: [Bool] -> Bool 
and' [] = True 
and' (x:xs)
    | x == False = False
    | otherwise = and' xs