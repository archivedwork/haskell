module Zip where
    -- zip'[] [True, False] == []
    -- zip'[1,2,3] [] == []
    -- zip'[1,2,3] [True, False] == [(1, True), (2, False)]
    -- zip'"abc" [1..] == [('a', 1), ('b', 2), ('c', 3)]

    zip' [] _ = []
    zip' _ [] = []
    zip' (x:xs) (y:ys) = (x,y) : zip' xs ys