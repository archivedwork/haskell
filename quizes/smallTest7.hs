
import Data.Char
import Data.List

allPositive :: [Int] -> Bool
-- allPositive [4, 5, 6, 8, 14]
-- allPositive [1..100]
-- allPositive []
-- not (allPositive [10, 9 ..])
-- not (allPositive [100, 98 .. 0])

allPositive [] = True
allPositive (x:xs) 
    | x > 0 = allPositive xs
    | otherwise = False




-- lowerCase [] == []
-- lowerCase "g" == "g"
-- lowerCase "aBc" == "ac"
-- lowerCase "Haskell" == "askell"
-- lowerCase "Functional langUAGES  " == "unctionallang"
-- lowerCase "c++ is object-oriented!" == "cisobjectoriented"
-- take 10 (lowerCase (cycle "haskell")) == "haskellhas"
lowerCase [] = []
lowerCase (x:xs) 
    | isLower(x) = x : lowerCase xs
    | otherwise  = lowerCase xs
