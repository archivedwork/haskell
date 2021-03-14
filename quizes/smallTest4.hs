{-

square 4
square 16
square 0
square 1
square 25
not (square 5)
not (square 12)
-}



squares :: [Int]
squares = [n^2 | n <- [0..]]

square :: Int -> Bool
square x
  | x `elem` squares  = True
  | otherwise         = False



powersOfTwo :: [Integer]
powersOfTwo = [2^n | n<-[0..]]


