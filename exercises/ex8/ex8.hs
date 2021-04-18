-- zip'[] [True, False] == []
-- zip'[1,2,3] [] == []
-- zip'[1,2,3] [True, False] == [(1, True), (2, False)]
-- zip'"abc" [1..] == [('a', 1), ('b', 2), ('c', 3)]


zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys 




-- unzip'[('a', 1), ('b', 2), ('c', 3)] == ("abc", [1,2,3])
-- unzip'[(1, True), (2, False)] == ([1,2], [True, False])
-- unzip'[] == ([],[])
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x,y):xs) = (x : fst(unzip' xs), y : snd (unzip' xs))



langAndRegion lst = (take 2 lst, drop 3 lst)