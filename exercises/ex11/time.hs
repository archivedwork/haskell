data Time = T Int Int
  deriving (Show)

showTime :: Time -> String
showTime (T h m) = show h ++ "." ++ show m




eqTime :: Time -> Time -> Bool
eqTime (T h1 m1) (T h2 m2)
  | h1 == h2 && m1 == m2    = True
  | otherwise               = False




isEarlier :: Time -> Time -> Bool
isEarlier (T h1 m1) (T h2 m2)
  | h1 <= h2 && m1 < m2   = True
  | h1 >= h2 || m2 <= m1  = False



{-
(either t1<t2<t3 or t3<t2<t1).
isBetween (T 12 30) (T 12 40) (T 12 50)
isBetween (T 12 50) (T 12 40) (T 12 30)
not (isBetween (T 12 30) (T 12 15) (T 12 50))
not (isBetween (T 12 30) (T 10 00) (T 12 50))
not (isBetween (T 12 30) (T 15 00) (T 12 50))-}

isBetween :: Time -> Time -> Time -> Bool
isBetween (T h1 m1)  (T h2 m2)  (T h3 m3) = isEarlier (T h1 m1) (T h3 m3) || isEarlier (T h3 m3) (T h1 m1)


data Timet = AM Int Int | PM Int Int
  deriving (Show)

showUSTime :: Timet -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"


addSquaresUpTo :: Int -> Int -> [Int]
addSquaresUpTo    n x = [n+(a*a) | a<- [0..x], a>=0, n+(a*a) <= x]


{-
removeLast even [0,1,2,3] == [0,1,3]
removeLast odd [0,1,2,3]  == [0,1,2]
removeLast even [1,3,5]   == [1,3,5]
-}


removeLast f []       = []
removeLast f (x:y:xs)
  | f y        =  x : removeLast f xs
  | otherwise  = removeLast f xs




{-
removeFirst even [0,1,2,3] == [1,2,3]
removeFirst odd  [0,1,2,3] == [0,2,3]
removeFirst even [1,3,5]   == [1,3,5]
-}

removeFirst f [] = []
removeFirst f (x:xs)
  | f x = xs
  | otherwise = x : removeFirst f xs



--f :: Num a => a -> Int -> [a] -> [[a]]
f n [] = []
f  n (x:xs) = [take n (x:xs)] ++ f n xs




-- swapElems 1 2 [1]               = [2]
-- swapElems 1 2 [2]               = [1]
-- swapElems 1 2 [3]               = [3]
-- swapElems 1 2 [1,9,5,3,2,1,5,2] = [2,9,5,3,1,2,5,1]

swapElems n1 n2 []   = []
swapElems n1 n2 (x:xs)
  | x == n1   =  n2 : swapElems n1 n2 xs
  | x == n2   =  n1 : swapElems n1 n2 xs
  | otherwise =   x : swapElems n1 n2 xs





-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.



allNaturals n = [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]


-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
fib 0 = [0]
fib 1 = [1]
fib 2 = [1]
fib n = ((n-1)+(n-2)) : fib (n-1)
