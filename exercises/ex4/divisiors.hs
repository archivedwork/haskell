-- divisors 10 == [1, 2, 5, 10]
-- divisors 16 == [1, 2, 4, 8, 16]
-- divisors 3  == [1, 3]


divisors n = [x | x <- [1..n], n `mod` x  == 0]