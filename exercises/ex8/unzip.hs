module Unzip where
    -- unzip'[('a', 1), ('b', 2), ('c', 3)] == ("abc", [1,2,3])
    -- unzip'[(1, True), (2, False)] == ([1,2], [True, False])
    -- unzip'[] == ([],[])

   -- unzip' :: [(a, a)] -> ([a], [a])
    unzip' [] = ([],[])
    unzip' l@((c,n):xs) = (getElement l, getElement1 l)


    getElement  [] = []
    getElement ((c,n):xs) = c : getElement xs

    getElement1  [] = []
    getElement1 ((c,n):xs) = n : getElement1 xs