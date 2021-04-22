import Data.List
import Data.Char

-- nub'[1,2,2,3,2,4] == [1,2,3,4]

deleteElem e [] = []
deleteElem e (x:xs)
    | e == x = deleteElem e xs
    | otherwise = x : deleteElem e xs

nub':: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (deleteElem x xs)



--concat'[[1,2], [3,4,5]] == [1,2,3,4,5]
concat' (x:xs) = x ++ concat' xs
concat' [] = []
