import Data.List
import Data.Char


firstTwo [True, True] = False
firstTwo [True, False, True] = True
firstTwo [False, True, True] = True
firstTwo [False, False, True, False] = False
firstTwo [True] = True
firstTwo [False] = False
firstTwo [] = False





wordNumWithCapital :: String -> Int
wordNumWithCapital "" = 0
wordNumWithCapital lst@(x:xs) = length $ filter (\x -> isUpper $ head(x)) (words lst)





test_minList = [
    minList [1..10] [2..11] == [1..10],
    minList [1..10] [] == [],
    minList [1, 2, 3, 4, 5] [5, 4, 3, 2, 1] == [1, 2, 3, 2, 1],
    minList [1, 2, 4, 1, 7] [3, 1, -10] == [1, 1, -10]]


minList :: [Int] -> [Int] -> [Int]
