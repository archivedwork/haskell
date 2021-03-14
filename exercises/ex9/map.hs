
-- map'(\n -> n + 2) [] == []
-- map'(\n -> n + 2) [2,3,4] == [4,5,6]
-- map'even [2,3,4] == [True, False, True]

map' f []            = []
map' f (x:xs)       = f x : map' f xs