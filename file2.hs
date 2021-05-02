
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
insert e [] = []
insert e (x:xs)
    | e < x        = e : x : insert  e xs
    | otherwise     = x : insert e xs



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