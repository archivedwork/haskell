module IsLeapYear where
    -- Check whether a year is a leap year. A year is considered a leap year if it can be divided by 4 but not 100. 
    -- However, years divisible by 400 are also leap years:
    -- •1992, 1996, 2012, 2016 are leap years, as they are divisible by 4 but not 100.
    -- •1700, 1800, 1900 are not leaps, as they are divisible by 4 and also 100.
    -- •1600, 2000 are leap years, as they are divisible 100 but also 400.
    
    -- isLeapYear 1992
    -- isLeapYear 1996
    -- isLeapYear 1600
    -- isLeapYear 2000
    -- not (isLeapYear 1700)
    -- not (isLeapYear 1800)



    isLeapYear x 
        | x `mod` 4 == 0 && x `mod` 100 /= 0 = True
        | otherwise                          = False