import Data.List
import Data.Char


-- Define a function `f2 :: Eq a => [a] -> [[a]]`
--   `f xs` should split the list `xs` between each pair of consecutive equal elements.
-- Examples:
--   f2 [1, 2, 3, 4] = [[1, 2, 3, 4]]
--   f2 [1, 2, 2, 1] = [[1, 2], [2, 1]]
--   f2 [1, 1, 1, 1] = [[1], [1], [1], [1]]
--   f2 [] = []



-- f [] = []
-- f lst@(x:y:xs)
--     | x == y    = [f_helper lst []]
--     | otherwise = [lst]

-- f_helper [] acc = acc
-- f_helper lst@(x:y:xs) acc 
--     | x == y       = acc ++ take x lst
--     | otherwise    = f_helper (y:xs) acc




-- Define a function `f :: (a -> Bool) -> [a] -> [[a]]`.
-- `f p xs` should be the list of the maximal continuous sublists of `xs` that
-- consist only of elements satisfying the predicate `p`.
 
-- Examples:
-- f even [0, 1, 2, 3] == [[0], [2]]
-- f odd [0, 1, 2, 3] == [[1], [3]]
-- f odd [0, 2, 4] == []
-- f even [0, 2, 1, 3] == [[0, 2]]
-- f odd [0, 2, 1, 3] == [[1, 3]]
-- f odd [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]

f p [] = []
f p lst@(x:xs)
    | p x        = [[x]] ++ f p xs
    | otherwise  = [even_helper p xs]


even_helper p [] = []
even_helper p (x:xs)
    | p x    =  x : even_helper p xs
    | otherwise = even_helper p xs


odd_helper p [] = []
odd_helper p (x:xs)
    | p x = x : odd_helper p xs
    | otherwise = []




-- merge3 [1, 2, 3] [7, 8, 9] [4, 5, 6] = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  -- merge3 [1, 4, 9] [2, 6, 8] [3, 5, 7] = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  -- merge3 [1, 2, 6] [3, 4, 5] []        = [1, 2, 3, 4, 5, 6]
  -- merge3 []        [1, 2, 3] []        = [1, 2, 3]

merge3 lst1 lst2 lst3 = sort $ lst1 ++ lst2 ++ lst3