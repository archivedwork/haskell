module Sort where
    -- sort [5, -2, 3] == [-2, 3, 5]
    -- sort []         == []

    sort [] = []
    sort (x:y:xs)
        | y <= x    = x : sort xs
        | otherwise = sort (y:xs) 