import Data.Char
import Data.List

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]


getIndexes lst = head [i | (v,i) <- zip lst [0..]]

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' n [] = []
elemIndices' n (x:xs)
    | n == x = [getIndexes (x:xs)]
    | otherwise = elemIndices' n xs

