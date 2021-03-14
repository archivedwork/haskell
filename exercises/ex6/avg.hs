
-- avg [1,2,3,4,5,6] == 3.5
-- avg [5,10] == 7.5
-- avg [100] == 100
-- avg [] == 0

avg [] = 0
avg xs = fromIntegral(sum (xs)) / fromIntegral(length (xs))