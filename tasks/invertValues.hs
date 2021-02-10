module InvertValues where


-- invert([1,2,3,4,5]) == [-1,-2,-3,-4,-5]
-- invert([1,-2,3,-4,5]) == [-1,2,-3,4,-5]
-- invert([]) == []

    -- first solution
    invert [] = []
    invert (x:xs) 
        | x < 0       = abs(x) : invert xs
        | otherwise   = (-x) : invert xs


    -- second solution
    invert1 xs = map (\x -> flipInt x) xs


    -- third solution
    invertRe [] = []
    invertRe (x:xs) = (flipInt x) : invertRe xs

    flipInt x = x * (-1)