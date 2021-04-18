
-- or'[True, True, True, True]
-- or'[True, True, False, True]
-- not (or'[])
-- not (or'[False, False, False, False])

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
    | x == True = True 
    | otherwise = or' xs