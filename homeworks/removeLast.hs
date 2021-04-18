import Data.List
import Data.Char



-- removeLast even [0, 1, 2, 3] == [0, 1, 3] 
-- removeLast odd  [0, 1, 2, 3] == [0, 1, 2] 
-- removeLast even [1, 3, 5]    == [1, 3, 5]
-- removeLast (> 0) []          == []

-- removeLast :: (a -> Bool) -> [a] -> [a]
-- removeLast p [] = []
-- removeLast p (x:xs)
--     | p (last(x:xs)) = init (x:xs)
--     | not $ p (last(x:xs)) = (x:xs)
--     | otherwise = []

removeLastHelper p [] = []
removeLastHelper p (x:xs)
    | p x       = xs
    | otherwise = x : removeLastHelper p xs

removeLast p [] = []
removeLast p (x:xs) = reverse $ removeLastHelper p (reverse (x:xs))




-- removeLastHelper :: (a -> Bool) -> [a] -> [a]
-- removeLastHelper f []=[]
-- removeLastHelper f (x:xs)
--  |f x ==True= xs
--  |otherwise=x:removeLastHelper f xs

-- removeLast :: (a -> Bool) -> [a] -> [a]
-- removeLast f []=[]
-- removeLast f (x:xs)=reverse(removeLastHelper f (reverse(x:xs)))

marksCorrect :: String -> Bool
-- marksCorrect ""
-- marksCorrect "x"
-- marksCorrect "Hello! You look great today"
-- marksCorrect "Howdy? I'm fine. Thanks"
-- not (marksCorrect "Oh! This looks bad.Dunno why")
-- not (marksCorrect "!")
-- not (marksCorrect "hello!")
-- marksCorrect "I'm sure this goes smoothly"


marksCorrect []                                        = True
marksCorrect "!"                                       = False 
marksCorrect "?"                                       = False 
marksCorrect "."                                       = False 
marksCorrect [x]                                       = True
marksCorrect (x:y:xs)
    | (x == '!' || x == '.' || x == '?') && (y == ' ') = marksCorrect xs
    | (x == '!' || x == '.' || x == '?') && (y /= ' ') = False
    | otherwise                                        = False
