-- f 3 [1,2,3,4] == [1,1,1,2,2,2,3,3,3,4,4,4]
f :: Int -> [Int] -> [Int]
f n (x:xs) = replicate n x ++ f n xs
f n [] = []

