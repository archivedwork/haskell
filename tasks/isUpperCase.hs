module IsUpperCase where 


-- isUpperCase :: [Char] -> Bool
import Data.Char

isUpperCase :: [Char] -> [Bool]
isUpperCase [] = []
isUpperCase (x:xs) = isUpper(x) : isUpperCase xs


isUpperCase2 :: [Char] -> Bool
isUpperCase2 str 
    | str == (map toUpper str) = True
    | otherwise = False