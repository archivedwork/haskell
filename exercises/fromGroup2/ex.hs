module Practice04 where

-- Conversions
--------------

-- We have several number types:
--   Int, Integer, Float, Double, ...

-- ceiling, floor, round :: (RealFrac a, Integral b) => a -> b
-- ceiling, floor, round :: Double -> Int
-- ceiling, floor, round :: Double -> Integer

-- fromInteger :: Integral b => Integer -> b
-- fromIntegral :: (RealFrac a, Integral b) => a -> b

-- fromIntegral :: Int -> Double
-- fromIntegral :: Integer -> Double
-- fromIntegral :: Int -> Integer
-- fromIntegral :: Integer -> Int

-- Compute the length of a vector with int coordinates.
vecLen :: (Int, Int) -> Double
vecLen = undefined

--- Compute the average of a list of integers.
average :: [Int] -> Double
average = undefined

-- More lists operations
------------------------

-- zip :: [a] -> [b] -> [(a, b)]
-- (e.g. zip :: [Int] -> [Char] -> [(Int, Char)])
-- zip [1, 2, 3] ['a', 'b', 'c'] == [(1, 'a'), (2, 'b'), (3, 'c')]

-- zip   [1,        2,        3]
--       ['a',      'b',      'c']
-- ==    [(1, 'a'), (2, 'b'), (3, 'c')]

-- Example:
filterEvenPos :: [Char] -> [Char]
filterEvenPos cs = [ c | (i, c) <- zip [0 .. length cs-1] cs, even i ]
-- filterEvenPos ['a', 'b', 'c', 'd', 'e'] == ['a', 'c', 'e']

-- zip [0 .. length cs-1] cs == zip [0..] cs

getNth :: Int -> [Char] -> Char
getNth = undefined
-- Examples:
--   getNth 0 ['a', 'b', 'c'] == 'a'
--   getNth 1 ['a', 'b', 'c'] == 'b'
--   getNth 2 ['a', 'b', 'c'] == 'c'
--   getNth 3 ['a', 'b', 'c']  is undefined

-- splitting lists:
--  take :: Int -> [a] -> [a]
--  drop :: Int -> [a] -> [a]
-- Example: 
--   take 5 [1..10] = [1,2,3,4,5]
--   drop 5 [1..10] = [6,7,8,9,10]

-- Appending lists:
--  (++)   :: [a] -> [a] -> [a]
--  concat :: [[a]] -> [a]
-- Examples:
--   [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
--   concat [ [1, 2], [3], [4, 5, 6] ] == [1,2,3,4,5,6]

-- Define a function rotate1 that rotates a list 1 step to the left. 
-- Examples:
--  rotate1 []           == []
--  rotate1 [1, 2, 3, 4] == [2, 3, 4, 1]
--  rotate1 [4, 2, 3, 1] == [2, 3, 1, 4]
rotate1 :: [Int] -> [Int]
rotate1 [] = []
rotate1  (x:xs) = reverse (rotate1 xs) ++ [x]

-- Define a function rotateN that rotates a list n steps to the left. 
-- Examples:
--   rotateN 2 [1, 2, 3, 4] == [3, 4, 1, 2]
--   rotateN 6 [1, 2, 3, 4] == [3, 4, 1, 2]
rotateN :: [Int] -> [Int]
rotateN (x:xs) = 
-- 
allSquares :: [Int]
allSquares = [ n * n | n <- [1..] ]

firstNSquares :: Int -> [Int]
firstNSquares n = take n allSquares

-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime = undefined

firstNPrimes :: Int -> [Int]
firstNPrimes = undefined

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at even and odd positions:
-- (You can assume that the length of the input list is even.)
--  Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos = undefined