-----------------------------------------
-- Define a function `f :: Eq a => [a] -> [Int]`.
--   `f xs` should replace each element `x` of `xs` by the number of previous
--   occurences of `x` in `xs`.
-- Examples:
--   f [1, 1, 1, 1]       = [0, 1, 2, 3]
--   f [1, 2, 3, 4]       = [0, 0, 0, 0]
--   f [1, 2, 1, 2, 2, 1] = [0, 0, 1, 1, 2, 2]
--   f []                 = []
-- f :: Eq a => [a] -> [Int]
-- f = undefined



-----------------------------------------
-- Define a function `f :: Eq a => [a] -> [[a]]`
--   `f xs` should split the list `xs` between each pair of consecutive equal elements.
-- Examples:
--   f [1, 2, 3, 4] = [[1, 2, 3, 4]]
--   f [1, 2, 2, 1] = [[1, 2], [2, 1]]
--   f [1, 1, 1, 1] = [[1], [1], [1], [1]]
--   f [] = []
-- f :: Eq a => [a] -> [[a]]
-- f = undefined




-- Define a function `f :: [Int] -> [[Int]]` that breaks a list of integers
-- into its non-decreasing continuous subsequences.

-- Examples:
--    f []        = []
--    f [0]       = [[0]]
--    f [1,2,3]   = [[1,2,3]]
--    f [3,2,1]   = [[3], [2], [1]]      
--    f [1,4,2,3] = [[1,4], [2,3]]
--    f [1,2,4,3] = [[1,2,4], [3]]
--    f [1,1,1]   = [[1,1,1]]



-- Define a function `f :: [Int] -> [Int]`.
--   `f` takes a list `xs` of integers, and returns a list consisting of the
--   elements of `xs` that are less than or equal to the previous element of
--   `xs`.

 

-- Examples:
--   f5 []           == []
--   f5 [1]          == [1]
--   f5 [1, 2, 3, 4] == [1]
--   f5 [4, 3, 2, 1] == [4, 3, 2, 1]
--   f5 [2, 1, 4, 3] == [2, 1, 3]     -- 4 is removed because 1 < 4
--   f5 [3, 4, 1, 2] == [3, 1]        -- 4 is removed because 3 < 4, 2 is removed because 1 < 2



-- Define a function `f :: (a -> Bool) -> [a] -> [[a]]`.
--   `f p xs` should be the list of the maximal continuous sublists of `xs` that
--   consist only of elements satisfying the predicate `p`.


-- Examples:
--   f even [0, 1, 2, 3] == [[0], [2]]
--   f odd  [0, 1, 2, 3] == [[1], [3]]
--   f even [0, 2, 1, 3] == [[0, 2]]
--   f odd  [0, 2, 1, 3] == [[1, 3]]
--   f odd  [0, 2, 4]    == []
--   f odd  [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]


f :: (a -> Bool) -> [a] -> [[a]]
f = undefined




 -- Define a function `f :: [Int] -> [Int]`.
--   The list `f xs` should be a sublist of `xs`. 
--   An element `x` of `xs` is included in `f xs` if it is 
--   greater than every element `y` that occurs later in the list `xs`.
-- Examples:
--   f [3,2,1]             == [3,2,1]
--   f [1,2,3]             == [3]
--   f [3,2,1,3,2,1]       == [3,3,2,1]
--   f [1,2,3,2,1]         == [3]
--
--   f [4,9,1,8,3,6,7,3,2] == [9,8,7,3,2]
-- f lst@(x:y:xs) 
--     | x > max x  y = []



