import Data.Char
import Data.List

test_hasDivisable = [
    hasDivisibleNum [] 4 == False,
    hasDivisibleNum [1..10] 5 == True,
    hasDivisibleNum [1,3..10] 5 == True,
    hasDivisibleNum [0,2..10] 5 == True,
    hasDivisibleNum [0,2..9] 5 == True,
    hasDivisibleNum [2,4,9] 3 == True,
    hasDivisibleNum [3,4,7,9,2,1] 10 == False,
    hasDivisibleNum [1,9,7,5,11,13] 2 == False,
    hasDivisibleNum [100..] 51 == True]

hasDivisibleNum :: Integral i => [i] -> i -> Bool
hasDivisibleNum [] n = False
-- hasDivisibleNum (x:xs) n
--     | True `elem` [ n `mod` a == 0 | a <- (x:xs)] = True
--     | otherwise = False

hasDivisibleNum (x:xs) n 
    | x `mod` n == 0 = True
    | otherwise = hasDivisibleNum xs n
    





-- greater 5 [] == 5
-- greater 10 [1..] == 11
-- greater 10 [2,4..] == 12
-- greater 'l' "hello" == 'o'
-- greater 'c' "hello" == 'h'
-- greater 'h' "haskell" == 's'
greater :: Ord a => a -> [a] -> a
greater n [] = n
greater n (x:xs)
    | x > n = x
    | otherwise = greater n xs
