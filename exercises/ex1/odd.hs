module Odd where 
    --Define a function which chcks whether its parameter isodd. Functionoddalready exists in Haskell, so lets callour function odd'.
    -- not (odd'2)
    -- odd'3
    -- not (odd'(-4))

    odd' x = (x `mod` 2 /= 0)


    -- or 
    odd1 x = (x `mod` 2 == 1)

    isEven x = (x `mod` 2 == 0)

    odd2 x = not(isEven x)