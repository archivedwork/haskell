module Drop where

    -- drop'4 "hello world"     == "o world"
    -- drop'4 ""                == ""
    -- drop'(- 2) "hello world" == "hello world"
    -- drop'8 [True, False]     == []
    -- drop'0 [True, False]     == [True, False]

    drop' :: Int -> [a] -> [a]
    drop' n [] = []
    drop' n lst | n < 0 = lst
    drop' n lst | n > length lst = []
    drop' n lst | n == 0 = lst
    drop' n (x:xs)
        | n <= length (x:xs)  = drop' (n-1) xs
        | otherwise           = x : drop' n xs