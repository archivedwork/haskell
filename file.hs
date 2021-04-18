import Data.Char
import Data.List

-- removeLast even [0, 1, 2, 3] == [0, 1, 3] 
-- removeLast odd  [0, 1, 2, 3] == [0, 1, 2] 
-- removeLast even [1, 3, 5]    == [1, 3, 5]
-- removeLast (> 0) []          == []

removeLast p [] = []
removeLast p (x:xs) = reverse $ removeLAstHelper p (reverse (x:xs)) []

removeLAstHelper p [] acc               = acc
removeLAstHelper p (x:xs) acc          
    | (p x)          =   acc ++ xs
    | otherwise      = x : removeLAstHelper p xs acc






-- surrounded [1, 3, 5, 5]       == [3]
-- surrounded [2, 4, 8, 10]      == [4, 8]
-- surrounded [4, 3, 5, 7, 4, 9] == [5]
-- surrounded [1]    == []
-- surrounded [1, 5] == []
-- surrounded []     == []
surrounded [] = []
surrounded [_] = []
surrounded [x,y] = []
surrounded (x:y:z:xs)
    | x < y && z > y       = [y] ++ surrounded (y:z:xs)
    | otherwise            =  surrounded (y:z:xs)





penultimate [] = 0
penultimate (x:xs)
    | length(xs) <= 1 = x
    | otherwise       = penultimate xs


