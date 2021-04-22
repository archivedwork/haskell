import Data.List
import Data.Char


-- filter'(\n -> n > 5) [] == []
-- filter'(\n -> n > 5) [1,2,5,6,0] == [6]
-- filter'even [1,2,5,6,0] == [2,6,0]
-- filter'(elem 0) [[5,6],[4,1,2,0],[0,5]] ==[[4,1,2,0],[0,5]]

filter' :: Num a => (a -> Bool) -> [a] -> [a]
filter' f []      = []
filter' f (x:xs)
    | f x        = [x]
    | otherwise   =  filter' f xs



upperToLower :: String -> String
upperToLower (x:xs)
    | isUpper(x) = toLower(x) : upperToLower xs
    | otherwise = upperToLower xs
upperToLower [] = []



-- all' :: (a->Bool) -> Bool
all' f (x:xs)
 | f x  = all' f xs
 | otherwise = False
all' f [] = True
