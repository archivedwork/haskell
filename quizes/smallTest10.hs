import Data.List
import Data.Char



-- kroeneckerDelta 0 0 == 1
-- kroeneckerDelta 0 1 == 0
-- kroeneckerDelta '#' '#' == 1
-- kroeneckerDelta '#' '@' == 0
-- kroeneckerDelta [1,2] [1,2] == 1
-- kroeneckerDelta [1,2] [3,4] == 0
kroeneckerDelta :: Eq a => a -> a -> Int
kroeneckerDelta x y
    | x == y = 1
    | otherwise = 0
    





-- deleteAll 5 [] == []
-- deleteAll 5 [5] == []
-- deleteAll 5 (10 : replicate 10 5) == [10]
-- deleteAll 5 [1..10] == [1..4] ++ [6..10]
-- deleteAll 5 [1..4] == [1..4]
-- deleteAll 'a' "almafa" == "lmf"
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll e [] = []
deleteAll e (x:xs)
    | e == x    = deleteAll e (xs)
    | otherwise = x : deleteAll e (xs)