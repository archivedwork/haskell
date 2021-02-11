module Pow where
    -- pow 0 2 == 0
    -- pow 0 0 == 1
    -- pow 2 0 == 1
    -- pow 2 1 == 2
    -- pow 3 2 == 9

    pow 0 x
        | x > 1 = 0
        | otherwise =1
    pow _ 0 = 1
    pow x y =  x * pow x (y-1)