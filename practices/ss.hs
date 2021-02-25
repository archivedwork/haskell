-- `countEven xs` should be the number of even elements in xs.
--countEven :: [Int] -> Int
countEven [] = []
countEven (x:xs)
  | isEven x      = x : countEven xs
  | otherwise     = countEven xs

countEven1 (x:xs) = length (countEven (x:xs))

isEven x = x `mod` 2 == 0