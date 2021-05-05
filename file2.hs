import Data.Char
import Data.List


-- [x..y]
countTo :: Int -> Int -> [Int]
countTo x  0 = []
countTo x y = x : countTo (x+1) (y-1)

countDown 0 y = [] 
countDown x y
    | x >= y = x : countDown (x-1) (y)
    | otherwise = countDown x y 





filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
    | even(x) = x : filterEven xs
    | otherwise = filterEven xs


mapInc :: [Int] -> [Int]
mapInc (x:xs) = (x+1) : mapInc xs
mapInc [] = []



take' n [] = []
take' n (x:xs) = x : take' (n-1) xs



drop' 0 lst = lst
drop' n (x:xs) = drop' (n-1) xs


-- insert 5 [1,4,10] == [1,4,5,10]
-- insert e [] = []
-- insert e (x:xs)
--     | e < x        = e : x : insert  e xs
--     | otherwise     = x : insert e xs



-- elemIndices 1 [1,2,3,1] == [0,3]
-- elemIndices a (x:xs) = [i | (v,i) <- zip (x:xs) [0..], a == v]

elemIndices a lst = getElemInd 0 a lst

getElemInd i a (x:xs)
    | x == a        = i : getElemInd (i+1) a xs
    | otherwise     = getElemInd (i+1) a xs
getElemInd i a [] = []



-- partition 5 [1,9,5,2,7] == ([1,2], [9,5,7])
-- partition a [] = ([], [])
-- partition a (x:xs)
--     | x < a = ((x : partition a xs), (partition a xs))
--     | otherwise = ((partition a xs), (x : partition a xs))
    


insertAtPred p n [] = [n]
insertAtPred p n (x:xs)
  | p x       = n : x  : xs
  | otherwise = x : insertAtPred p n xs

addSquaresUpTo n x | n < x = [n+a*a | a <- [n..x-1], (n+a*a) <= x, a >= 0]
addSquaresUpTo n x | n > x = []


-- -- f :: [(Int, a)] -> [a]
-- f ((x,y):xs) = replicate x y ++ f xs
-- f [] = []

f "" _  = []
f (x:xs) (y:ys)
    | x == y = x : f xs ys
    | otherwise = f xs ys

ff lst1 lst2 = length $ f lst1 lst2
  -- [ | (v1,i1) <- zip lst1 [0..],  (v2, i2) <- zip lst2 [0..], v1 == v2]



fx n [] = []
fx n (x:xs)
    | length (take n (x:xs)) <= n =  [take n (x:xs)] ++  [xs]
    | otherwise = [drop n (x:xs)] ++ fx n xs


--   f ""           == []
--   f "abc"        == ["abc"]
--   f "abcABC"     == ["abc", "ABC"]
--   f "aAbBcC"     == ["a", "A", "b", "B", "c", "C"]
--   f "1234567890" == []
--   f "a1b2c3"     == ["abc"]

fm [] = []
fm (x:xs)
    | isLower(x)   =  [x] : fm xs
    | isUpper (x)  = [x] : fm xs
    | otherwise    = fm xs


everyThird "" = ""
everyThird (x:y:z:xs) = z : everyThird xs


myElem e [] = False
myElem e (x:xs)
    | e == x = True
    | otherwise = myElem e xs

value n [] = []
value n ((v, lst):xs)
    | n == v = lst
    | otherwise = "scala"


replic 0 x = []
replic n x = x : replic (n-1) x


-- inst n [] = []
inst n [] = [n]
inst n (x:xs)
    | n <= x       = (n : x : xs)
  --  | n > x        = (x:xs) ++ [n]
    | otherwise    = x : inst n xs

format n [] = []
format n (x:xs)
    | length ((x:xs)) < n    = replicate (n - (length ((x:xs)))) ' ' ++  (x:xs)
    | otherwise = (x:xs)

insert' n [] = [n]
insert' n  (x:xs)
  | n <= x    = n : x : xs
  | otherwise = x : insert' n xs


srt [] = []
srt (x:y:xs) = sort $ insert' x (y:xs)


breakOn c [] = ([], [])
breakOn c (x:xs) = (takeWhile (\x -> x /= c) (x:xs), dropWhile (\x -> x /= c) (x:xs))




-- Examples:
--   swapElems 1 2 [1]               = [2]
--   swapElems 1 2 [2]               = [1]
--   swapElems 1 2 [3]               = [3]
--   swapElems 1 2 [1,9,5,3,2,1,5,2] = [2,9,5,3,1,2,5,1]


swapElem x y [] = []
swapElem x y (a:as)
  | x == a         = y : swapElem x y as
  | y == a         =  x : swapElem x y as
  | otherwise  = a : swapElem x y as



--   duplicatePred odd  [1, 2, 3] = [1, 1, 2, 3, 3]
--   duplicatePred even [1, 2, 3] = [1, 2, 2, 3]
--   duplicatePred odd  [2, 4, 6] = [2, 4, 6]
duplicatePreds p [] = []
duplicatePreds p (x:xs)
   | p x                  = x : x : duplicatePreds p xs
   | otherwise    = x : duplicatePreds p xs



ndaysInMonth :: [Int]
ndaysInMonth = [ 31, 28, 31, 30, 31, 30
              , 31, 31, 30, 31, 30, 31 ]

-- nextDay :: Date -> Date
-- nextDay = undefined

-- Examples:
--   nextDay {{01-01-2000}} == {{02-01-2000}}
--   nextDay {{30-05-2000}} == {{31-05-2000}}
--   nextDay {{30-06-2000}} == {{01-07-2000}}
--   nextDay {{31-12-2000}} == {{01-01-2001}}




--   f even [0, 1, 2, 3] == [[0], [2]]
--   f odd  [0, 1, 2, 3] == [[1], [3]]
--   f even [0, 2, 1, 3] == [[0, 2]]
--   f odd  [0, 2, 1, 3] == [[1, 3]]
--   f odd  [0, 2, 4]    == []
--   f odd  [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]


f4 p [] = []
f4 p (x:xs)
    | p x  = x : f4 p xs
    | otherwise = f4 p xs

myFun p lst
    | sort lst == lst    = group $ f4 p lst
    | otherwise          = [f4 p lst]





--   f5 []           == []
--   f5 [1]          == [1]
--   f5 [1, 2, 3, 4] == [1]
--   f5 [4, 3, 2, 1] == [4, 3, 2, 1]
--   f5 [2, 1, 4, 3] == [2, 1, 3]     -- 4 is removed because 1 < 4
--   f5 [3, 4, 1, 2] == [3, 1]        -- 4 is removed because 3 < 4, 2 is removed because 1 < 2


f5 [] = []
f5 [x] = [x]
f5 (x:y:xs)
  | sort (x:y:xs) == (x:y:xs) = [x]
  | x <= y   = x : f5 (xs) 
  | otherwise =  x  : f5 (y:xs)