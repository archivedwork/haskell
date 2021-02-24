module Take where

    --  take'4 "hello world"     == "hell"
    -- take'4 ""                 == ""
    -- take'(- 2) "hello world" == ""
    -- take'8 [True, False]     == [True, False]
    -- take'0 [True, False]     == []

    take' :: Int -> [a] -> [a]
    take' n []                   = []
    take' n _ | n < 0            = []
    take' n _ | n == 0           = []
    take' n lst | n > length lst = lst
    take' n (x:xs)
        | n < length (x:xs)   = x : take' (n-1) xs
        | otherwise           = take' n xs
    