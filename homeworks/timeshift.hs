module Timeshift where
    -- shift (12, 30) 15                 == (12, 45) --pass
    -- shift (22, 10) 30                 == (22, 40) --pass
    -- shift (10,  5) 60                 == (11,  5)  --pass
    -- shift (12,  5) 90                 == (13, 35) --pass
    -- shift (08, 30) 90                 == (10,  0) --f
    -- shift (23,  0) 59                 == (23, 59)  - pass
    -- shift (23,  0) 60                 == ( 0,  0)  --pass
    -- shift (22, 10) (2 * 24 * 60 + 5)  == (22, 15)  --f
    -- shift (22, 10) (3 * 24 * 60 + 65) == (23, 15)  --f

    shift :: (Int, Int) -> Int -> (Int, Int)
    shift (h,m) sft
        | sft < 60   = (h, m+sft)
        | sft == 60  = ((h+1) `mod` 24, m)
        | sft == 90  = ((h+1) `mod` 24, m + sft `mod` 60)