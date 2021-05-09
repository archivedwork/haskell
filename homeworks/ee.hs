import Data.List
import Data.Char


surrounded :: [Int] -> [Int]
surrounded [] = []
surrounded [x] = []
surrounded [x,y] = []
surrounded (x:y:z:xs)
    | y > x && y < z   = y : surrounded (y:z:xs)
    | otherwise = surrounded (y:z:xs)

test_surrounded = [
    surrounded [1, 3, 5, 5]       == [3],
    surrounded [2, 4, 8, 10]      == [4, 8],
    surrounded [4, 3, 5, 7, 4, 9] == [5],
    surrounded [1]    == [],
    surrounded [1, 5] == [],
    surrounded []     == []]





removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f [] = []
removeLast f lst = reverse $ removeLastHelper f (reverse lst)


removeLastHelper f [] = []
removeLastHelper f (x:xs)
    | f x       = x : removeLastHelper f xs
    | otherwise =  removeLastHelper f xs


-- test_removeLast = [
--     removeLast even [0, 1, 2, 3] == [0, 1, 3],
--     removeLast odd  [0, 1, 2, 3] == [0, 1, 2],
--     removeLast even [1, 3, 5]    == [1, 3, 5],
--     removeLast (> 0) []          == []]


expand :: [(Int, a)] -> [a]

 --   expand [(1,'a'), (0,'b'), (2,'c')] == "acc"
 --   expand [(0,'a'), (1,'b'), (2,'c')] == "bcc"
 --   expand [(1,'a'), (0,'b'), (2,'c')] == "acc"
 

--   expand [(0,True), (0,False)] == []
 --   expand [] == []

expand [] = []
expand [(0, _)] = []
expand ((n, c):xs) = replicate n c ++ expand xs



oddIx :: [Int] -> [Int]
oddIx [] = []
oddIx lst = [i | (i,v) <- zip [0..] lst, odd v]
