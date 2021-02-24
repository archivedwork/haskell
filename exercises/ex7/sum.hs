module Sum where
    -- sum'[4,5,6] == 15
    -- sum'[]      == 0
    -- sum'[1..10] == 55

    sum1 [] = 0
    sum1 (x:xs) = x + sum1 xs 