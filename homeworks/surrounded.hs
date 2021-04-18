import Data.Char
import Data.List


-- surrounded [1, 3, 5, 5]       == [3]
-- surrounded [2, 4, 8, 10]      == [4, 8]
-- surrounded [4, 3, 5, 7, 4, 9] == [5]
-- surrounded [1]    == []
-- surrounded [1, 5] == []
-- surrounded []     == []


surrounded [] = []
surrounded [x] = []
surrounded [x,y] = []
surrounded (x:y:z:xs)
    | x < y && y < z    = y : surrounded (x:z:xs)
    | otherwise = surrounded (y:z:xs)