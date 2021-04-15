import Data.Char
import Data.List



-- addSeconds [5,2,0,5,7,9] == 16
-- addSeconds [9,4,2] == 4
-- addSeconds [10] == 0
-- addSeconds [] == 0



addSeconds :: [Integer] -> Integer
addSeconds [] = 0
addSeconds [x] = 0
addSeconds (x:y:xs) = y + addSeconds (xs)




-- penultimate [2,0,9]   == 0
-- penultimate [1..101]  == 100
-- penultimate [10,8..1] == 4


penultimate :: [Int] -> Int
penultimate [] = 0
penultimate (x:xs) = head( penHelp (x:xs) [])



penHelp [] acc = acc
penHelp lst acc = [head (tail (reverse(lst)))] ++ acc ++ penHelp (tail lst) acc 