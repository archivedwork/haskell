-- isAbundant 2  == False
-- isAbundant 4  == False
-- isAbundant 12 == True
-- isAbundant 14 == False
-- isAbundant 18 == True
-- isAbundant 20 == True




    -- A positive integer is called abundant number, if the sum of all of its proper divisors is greater than the number itself. The proper divisors of an integer excludes the integer itself.

    -- Define a function that checks whether an integer is abundant.

    -- Hint: function sum can sum up a list of numbers.


import Data.List
import Data.Char

isDivisors num = [x | x <- [1..(num-1)], num `rem` x == 0]


isAbundant x = sum (isDivisors x) > x

