-- filter'(\n -> n > 5) [] == []
-- filter'(\n -> n > 5) [1,2,5,6,0] == [6]
-- filter'even [1,2,5,6,0] == [2,6,0]
-- filter'(elem 0) [[5,6],[4,1,2,0],[0,5]] ==[[4,1,2,0],[0,5]]

filter' :: Num a => (a -> Bool) -> [a] -> [a]
filter' f []      = []
filter' f (x:xs)
    | f x        = [x]
    | otherwise   =  filter' f xs