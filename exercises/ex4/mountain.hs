module Mountain where
    -- mountain 3 == [1, 2, 3, 2, 1]
    -- mountain 5 == [1, 2, 3, 4, 5, 4, 3, 2, 1]
    mountain x = [x | x <- [1..x]] ++ reverse [x | x <- [1..x-1]]
    