module ModDiv where
    -- modDiv 10 5 == (0,2)
    -- modDiv 11 5 == (1,2)
    -- modDiv 7 7  == (0,1)
    -- modDiv 7 8  == (7,0)
    -- modDiv 7 1  == (0,7)

    modDiv x y =  ((mod x y),(div x y))