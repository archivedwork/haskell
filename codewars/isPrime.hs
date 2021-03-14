module IsPrime where


import Data.List
import Data.Char

prime x 
 --   | x > 1 && x `div` x == 1 && x `mod` 1 == 0  = True
    | factors x == [1,x] = True
    | otherwise = False

factors n = [x | x <- [1..n], mod n x == 0]



getCount = length . filter (`elem` "aeiou")


digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)

fromDigits (x:xs) = aux (x:xs) 0

aux [] acc     = acc
aux (x:xs) acc = aux xs ((acc * 10) + x)

aux1 xs acc = foldl (\ acc x -> (acc * 10) + x) acc xs


decendingOrder = fromDigits . reverse . sort . digits



divisors n = [x | x <- [1..n], mod n x == 0]



-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime n
    | products n == [1,n] = True 
    | otherwise           = False 

products n = [x | x <- [1..n], mod n x == 0]


firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [p | p <- [1..], isPrime p]




-- Define a function rotateN that rotates a list n steps to the left. 
-- Examples:
--   rotateN 2 [1, 2, 3, 4] == [3, 4, 1, 2]
--   rotateN 6 [1, 2, 3, 4] == [3, 4, 1, 2]
rotateN :: Int -> [Int] -> [Int]
rotateN n xs
    | n < length xs   = drop n xs ++ take n xs
    | otherwise       = drop (n `mod` length(xs)) xs ++ take (n `mod` length(xs)) xs