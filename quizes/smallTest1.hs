module SmallTest where

    -- secondsInDays 1 == 86400
    -- secondsInDays 0 == 0
    -- secondsInDays 2 == 172800
    -- secondsInDays 10 == 864000
    
    num = 86400

    secondsInDays x = num * x


    -- Define a function that checks whether an integer is divisible by 3 and 7 but not by 5.
    -- divisible 1 == False
    -- divisible 3 == False
    -- divisible 5 == False
    -- divisible 21 == True
    -- divisible 63 == True
    -- divisible 105 == False
    -- divisible (-21) == True

    divisable x
        | x `mod` 3 == 0 && x `mod` 7== 0  && x `mod` 5 /= 0   = True 
        | otherwise           = False