import Data.Char
import Data.List
import Data.Tuple

-- Split a list into chunks of length $n &gt; 0$. Only the last chunk is allowed to be shorter than $n$. (2 points)
runs :: [a] -> Int -> [[a]]
runs [] _ = []
runs lst@(x:xs) n = [take n lst] ++ runs (drop (length (take n lst)) lst) n


-- runs [] 9 == []
-- runs [1,4,9,16,25,36,49,64,81,100] 3 == [[1,4,9],[16,25,36],[49,64,81],[100]]
-- runs ["apple", "plum", "peach"] 1  == [["apple"], ["plum"], ["peach"]]
-- runs ["apple", "plum", "peach", "currant", "gooseberry"] 2 == [["apple", "plum"], ["peach", "currant"], ["gooseberry"]]
-- runs [0..23] 5 == [[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14],[15,16,17,18,19],[20,21,22,23]]





-- Capitalize the first letters of all words and convert everything else to small case. (2 points)
titleCase :: String -> String
--titleCase [] = []
titleCase lst@(x:xs) =  unwords $ [ toUpper h : map toLower tl | as <- words lst, h <- [head as], tl <- [tail as]]

-- titleCase "Haskell" == "Haskell"
-- titleCase "haskeLl" == "Haskell"
-- titleCase "a tRee With APPLES" == "A Tree With Apples"
-- titleCase "a" == "A"
-- titleCase "" == ""

mapToUpper lst = map (\x -> head x) $ words lst

mapToLower lst = map (\x -> toLower x) $ unwords $ map (\x -> tail x) $ words lst





-- Collect elements that are sandwiched between a smaller element from the left and a larger element from the right. (2 points)
surrounded :: [Int] -> [Int]
surronded [] = []
surrounded [x] = []
surrounded [x,y] = []
surrounded lst@(x:y:z:xs)
    | x < y && y < z   = y : surrounded (y:z:xs)
    | otherwise        = surrounded (y:z:xs)

-- surrounded [1, 3, 5]     == [3]
-- surrounded [2, 4, 8, 10] == [4, 8]
-- surrounded [4, 3, 5, 9]  == [5]
-- surrounded [1]   == []
-- surrounded [1,5] == []
-- surrounded []    == []






-- Check whether all integers in a list have the same sign. $0$ can be positive and negative here, we ignore it. (2 points)
sameSign :: [Int] -> Bool
sameSign [0,_] = True
sameSign [] = True
sameSign lst@(x:y:xs)
    | length (lst) == length(helper_neg lst)  = True
    | length (lst) == length(helper_pos lst)  = True
    | otherwise = False



helper_neg lst = filter (\x -> x == (-1) || x == 0) $ map (\x -> signum x) lst
helper_pos lst = filter (\x -> x == (1) || x == 0) $ map (\x -> signum x) lst


-- sameSign []
-- sameSign [-1, -12, -123]
-- sameSign [3, 7, 0, 32]
-- sameSign [0, -1]
-- sameSign [0, 0]
-- not (sameSign [-1, 1, 2, 3])
-- not (sameSign [-10..10])







-- range :: Number -> Number -> [Number]
-- range (Odd a )  (Odd b)
--  |a>b=[]
--  |a==b && a `mod` 2 ==0 =[Even a]
--  |a==b && a `mod` 2 /=0 =[Odd a]
--  |a `mod` 2 ==0=[Even a]++ range (Odd (a+1)) (Odd b)
--  |a `mod` 2 /=0=[Odd a]++ range (Odd (a+1)) (Odd b)
-- range (Odd a )  (Even b)
--  |a>b=[]
--  |a==b && a `mod` 2 ==0 =[Even a]
--  |a==b && a `mod` 2 /=0 =[Odd a]
--  |a `mod` 2 ==0=[Even a]++ range (Odd (a+1)) (Even b)
--  |a `mod` 2 /=0=[Odd a]++ range (Odd (a+1)) (Even b)
-- range (Even a )  (Odd b)
--  |a>b=[]
--  |a==b && a `mod` 2 ==0 =[Even a]
--  |a==b && a `mod` 2 /=0 =[Odd a]
--  |a `mod` 2 ==0=[Even a]++ range (Even (a+1)) (Odd b)
--  |a `mod` 2 /=0=[Odd a]++ range (Even (a+1)) (Odd b)
-- range (Even a )  (Even b)
--  |a>b=[]
--  |a==b && a `mod` 2 ==0 =[Even a]
--  |a==b && a `mod` 2 /=0 =[Odd a]
--  |a `mod` 2 ==0=[Even a]++ range (Even (a+1)) (Even b)
--  |a `mod` 2 /=0=[Odd a]++ range (Even (a+1)) (Even b)
data Number = Even Int
                | Odd  Int
      deriving (Eq, Show)


range :: Number -> Number -> [Number]
range (Even x) (Odd y) = map (\x -> if even x then (Even x) else (Odd (x))) [x..y]
range (Odd x) (Even y) = map (\x -> if odd x then (Odd x) else (Even (x))) [x..y]
range (Odd x) (Odd y)  = [Odd x] 
range (Even x) (Even y) = [Even x]






-- Create an interval of Numbers. The interval is closed on both ends, meaning both ends should be in the list. (2 points)

--     The first parameter may be greater than the second. In that case, the result should be empty.

--     range :: Number -> Number -> [Number]

--     range (Even 0) (Odd 3)  == [Even 0, Odd 1, Even 2, Odd 3]
--     range (Odd 5) (Even 8)  == [Odd 5, Even 6, Odd 7, Even 8]
--     range (Odd 5) (Odd 5)  == [Odd 5]
--     range (Even 4) (Even 4) == [Even 4]
--     range (Even 4) (Odd 3)  == []




--   f ""           == []
--   f "abc"        == ["abc"]
--   f "abcABC"     == ["abc", "ABC"]
--   f "aAbBcC"     == ["a", "A", "b", "B", "c", "C"] 
--   f "1234567890" == []
--   f "a1b2c3"     == ["abc"]

abc  = ['a'..'z']
capABC = ['A'..'Z']
numb = [0..9]

f [] = []
f lst@(x:xs)
    | x `elem` abc   =  [smHelper lst] ++ f (drop (length(smHelper lst)) lst)
    | x `elem` capABC = [capHelper lst] ++ f (drop (length (capHelper lst)) lst)
    | otherwise = []



smHelper [] = []
smHelper lst@(x:xs)
    | x `elem` abc = x : smHelper xs
    | x `elem` ['0'..'9'] = smHelper xs
    | otherwise = [] 
capHelper lst@(x:xs)
    | x `elem` capABC   =  x : capHelper xs
    | x `elem` ['0'..'9']     = capHelper xs
    | otherwise         = [] 
capHelper [] = []






-- Define a higher-order function `f :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]`
--   that uses all of its arguments.
f :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]
-- f             f1                 f2         lst    = [(c,b)]        -- f2 (head lst) (head lst)
f   f1 f2 lst  = [swap (f1 (head lst, f2 (head lst) (head lst)))]



-- Define a type Time, that can store times of the day in the format (HH AM/HH PM).
--   (e.g. 12 AM,  01 AM,  12 PM,  07 PM, ...)
-- 2. Define a function `nextTime :: Time -> Time` that advances a given time of the day by 1 hour.
--  nextTime 11 PM == 12 AM (or 00 AM if you want)

type HH = Int

data Time = AM HH | PM HH deriving (Show, Eq)

-- type HH = Int
-- data Time =  HH AM |  HH PM deriving (Show, Eq)



-- nextTime :: Time -> Time 
-- nextTime (h PM) = 
--     case h of
--         11 -> 12 AM
--         _ -> (h+1) PM

-- nextTime (h AM) =
--     case h of 
--         12 -> (1 PM)
--         _ -> ((h+1) AM)

    



-- Collect all elements with multiple occurences in a list. That is, remove elements which occur only once. (2 points)
multiple :: [Char] -> [Char]
-- multiple [] = []
multiple [x] = []
multiple lst@(x:y:xs) = likeCount x (y:xs) ++ multiple (y:xs)


likeCount a [] = []
likeCount a (x:xs)
    | a == x   = a : likeCount a xs
    | otherwise = likeCount a xs


-- multiple ""            == ""
-- multiple "haskell"     == "l"
-- multiple "erlang"      == ""
-- multiple "scala"       == "a"
-- multiple "c++"         == "+"
-- multiple "shakespeare" == "sae"










-- Define the reverse of map. It takes an element and a list of functions. It applies each function over the element. (2 points)
reverseMap :: a -> [a -> b] -> [b]
reverseMap e lst@(f:fs) = f e : reverseMap e fs
reverseMap e [] = []

-- reverseMap 5 [(+1), (*2), (7-)] == [6, 10, 2]
-- reverseMap 3 [\n -> take n [1..], \n -> replicate n 0, \n -> drop n [1..3]] == [[1..3], [0,0,0], []]





fw :: (a -> b -> a -> c) -> (a -> b) -> [a] -> [c]
fw    f1                      f2        lst = [f1 (head lst) (f2 (head lst)) (head lst)]



-- Define a function `fx :: Int -> [a] -> [[a]]`.
--`f n xs` should breaks the list `xs` into chunks of size `n`.

-- Examples:
--   fx 1 [1, 2, 3, 4] == [[1], [2], [3], [4]]
--   fx 2 [1, 2, 3, 4] == [[1, 2], [3, 4]]
--   fx 3 [1, 2, 3, 4] == [[1,2,3], [4]]
--   fx 4 [1, 2, 3, 4] == [[1,2,3,4]]

fx n lst@(x:xs) = [take n lst] ++ fx n (drop (length (take n lst)) lst)
fx n [] = []







-- Define a function `f :: [Int] -> [Int]`.
--   `f` takes a list `xs` of integers, and returns a list consisting of the
--   elements of `xs` that are less than or equal to the previous element of
--   `xs`.

 

-- Examples:
--   f5 []           == []
--   f5 [1]          == [1]
--   f5 [1, 2, 3, 4] == [1]
--   f5 [4, 3, 2, 1] == [4, 3, 2, 1]
--   f5 [2, 1, 4, 3] == [2, 1, 3]     -- 4 is removed because 1 < 4
--   f5 [3, 4, 1, 2] == [3, 1]        -- 4 is removed because 3 < 4, 2 is removed because 1 < 2

f5 [] = []
f5 [x] = [x]
f5 lst@(x:y:xs)
    | x <= y    = x : f5 (y:xs)
   -- | x > y     = lst-
    | otherwise =  f5 (y:xs)






-- Define a function `f :: (a -> Bool) -> [a] -> [[a]]`.
--   `f p xs` should be the list of the maximal continuous sublists of `xs` that
--   consist only of elements satisfying the predicate `p`.


-- Examples:
--   f even [0, 1, 2, 3] == [[0], [2]]
--   f odd  [0, 1, 2, 3] == [[1], [3]]
--   f even [0, 2, 1, 3] == [[0, 2]]
--   f odd  [0, 2, 1, 3] == [[1, 3]]
--   f odd  [0, 2, 4]    == []
--   f odd  [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]


-- f :: (a -> Bool) -> [a] -> [[a]]
-- f        p    lst@(x:xs) = helper p lst



-- helper 





-- Define a function `f2 :: Eq a => [a] -> [[a]]`
--   `f xs` should split the list `xs` between each pair of consecutive equal elements.
-- Examples:
--   f2 [1, 2, 3, 4] = [[1, 2, 3, 4]]
--   f2 [1, 2, 2, 1] = [[1, 2], [2, 1]]
--   f2 [1, 1, 1, 1] = [[1], [1], [1], [1]]
--   f2 [] = []

helper [y] = [y]
helper lst@(x:xs)
    | x == (head xs)   = x:[]
    | otherwise = x : helper xs

f2 lst@(x:xs) = [helper lst] ++ f2 (drop (length (helper lst)) lst)
f2 [] = []



-- Define a function `f :: (a -> Bool) -> [a] -> [[a]]`.
--   `f p xs` should be the list of the maximal continuous sublists of `xs` that
--   consist only of elements satisfying the predicate `p`.


-- Examples:
--   f even [0, 1, 2, 3] == [[0], [2]]
--   f odd  [0, 1, 2, 3] == [[1], [3]]
--   f even [0, 2, 1, 3] == [[0, 2]]
--   f odd  [0, 2, 1, 3] == [[1, 3]]
--   f odd  [0, 2, 4]    == []
--   f odd  [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]


fHelper p [] = []
fHelper p lst@(x:xs)
    | p x         = x : fHelper p xs
    | otherwise   = fHelper p xs

fe :: (a -> Bool) -> [a] -> [[a]]
fe p [] = []
fe        p    lst@(x:xs) 
    | p x =  [fHelper p lst] ++ fe p (drop (length (fHelper p lst)) xs)
    | otherwise = fe p (drop (length (fHelper p lst)) xs)








getNumber:: Number ->Int
getNumber (Even x) = x 
getNumber (Odd x) = x 
 
addNumber:: Int-> Number
addNumber x 
            | even x = Even x
            |otherwise  = Odd x
 
range :: Number -> Number -> [Number]
range x y =  [   addNumber x  | x <- [(getNumber x).. getNumber y]   ]
