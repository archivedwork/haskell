import Data.List
import Data.Char
import Data.Tuple

-- Expand each pair in a list so that an element e occurs n times in the result. (2 points)

--     expand [(1,'a'), (0,'b'), (2,'c')] == "acc"
--     expand [(0,'a'), (1,'b'), (2,'c')] == "bcc"
--     expand [(1,'a'), (2,'b'), (0,'c')] == "abb"
--     expand [(0,True), (0,False)] == []
--     expand [] == []

expand :: [(Int, a)] -> [a]
expand ((n, c):xs) = replicate n c ++ expand xs
expand [] = []



-- Enumerate indices of odd numbers in a list. (2 points)

--     oddIx [1]   == [0]
--     oddIx [1,2] == [0]
--     oddIx []    == []
--     oddIx [0..10] == [1,3,5,7,9]
--     oddIx [1..10] == [0,2,4,6,8]
--     oddIx [1,4,6,7,3,14,8] == [0,3,4]


oddIx :: [Int] -> [Int]
oddIx [x] = [0]
oddIx lst = [i | (i,v) <- zip [0..] lst, odd v]



-- Add the index of each element to the element itself. (2 points)
--     addIndex []  == []
--     addIndex [0] == [0]
--     addIndex [0,0,0,0] == [0..3]
--     addIndex [0..5]    == [0,2..10]
--     addIndex [1,5,4]   == [1,6,6]


addIndex :: [Int] -> [Int]
addIndex lst = [i+v | (i,v) <- zip [0..] lst]




data Number = Even Int | Odd  Int deriving (Eq, Show)

--  Create an interval of Numbers. The interval is closed on both ends, meaning both ends should be in the list. (2 points)

--     The first parameter may be greater than the second. In that case, the result should be empty.


--     range (Even 0) (Odd 3)  == [Even 0, Odd 1, Even 2, Odd 3]
--     range (Odd 5) (Even 8)  == [Odd 5, Even 6, Odd 7, Even 8]
--     range (Odd 5) (Odd 5)  == [Odd 5]
--     range (Even 4) (Even 4) == [Even 4]
--     range (Even 4) (Odd 3)  == []
-- took 5 min 
range :: Number -> Number -> [Number]
range (Even x) (Odd y) | x > y =  []
range (Even x) (Even y) = [Even x]
range (Odd x) (Odd y)  = [Odd x]
range (Even x) (Odd y) | x < y = map (\x -> if even x then (Even x) else (Odd x)) [x..y]
range (Odd x) (Even y) | x < y = map (\x -> if odd x then (Odd x) else (Even x)) [x..y]




-- Define function composition of a list of functions. (3 points)

--     pipeline [f1,f2, ... , fn]   = f1 . f2 . ... . fn
--     pipeline [f1,f2, ... , fn] x = f1 (f2 (... (fn x)))


--     pipeline [(*2), (+1), (*3)] 1 == 8
--     pipeline [(+1), (*2), (*3)] 1 == 7
--     pipeline [(*2), (*3), (+1)] 1 == 12
--     pipeline [map (*2), take 3, drop 2] [1..10] == [6,8,10]

pipeline :: [a -> a] -> (a -> a)
pipeline (p1:p2:p3:ps) n   = p1 (p2 (p3 n)) 




-- From index $i$ take $n$ elements from a list. Indexing starts with 0. We assume index $i$ and integer $n$ are nonnegative. (1 point)

-- sublist 1 4 "Haskell"  == "aske"
-- sublist 10 4 "Haskell" == ""
-- sublist 2 0 "Little Brown Fox..."   == ""
-- sublist 16 10 "Little Brown Fox..." == "..."


sublist :: Int -> Int -> [a] -> [a]
sublist n1 n2 lst@(x:xs)
    | n1 <= length lst = take n2 (drop n1 lst) 
    | n1 > length lst = []






-- Define integer multiplication recursively with addition and subtraction only. (2 points)
mul :: Integer -> Integer -> Integer
mul  0 _ = 0
mul _ 0  = 0
mul n1 n2 = n1 + mul (n1) (n2-1)

-- mul 0 2   == 0
-- mul 5 0   == 0
-- mul 5 6   == 30
-- mul 10 12 == 120


-- Count words starting with upper case letters. (2 points)
-- wordWithCapital "Every Word Starts With Upper Case Letter" == 7
-- wordWithCapital "Here not Every one Starts With capital"   == 4
-- wordWithCapital "thiS hAs SoME weiRd words As weLL"        == 2


wordWithCapital :: String -> Int
wordWithCapital lst@(x:xs) = length $ filter (\x -> isUpper (head x)) $ words lst


-- Define a function minList which traverses two lists simultaneously and takes the smaller element at each position. (2 points)
minList :: [Int] -> [Int] -> [Int]
minList lst1@(x:xs) lst2@(y:ys) 
    | x <= y      =  x : minList xs ys
    | otherwise   = y : minList xs ys
minList _ [] = []
minList [] _ = []
-- minList [1..10] [2..11] == [1..10]
-- minList [1..10] []      == []
-- minList [1, 2, 3, 4, 5] [5, 4, 3, 2, 1] == [1, 2, 3, 2, 1]
-- minList [1, 2, 4, 1, 7] [3, 1, -10]     == [1, 1, -10]



-- Check whether every punctuation mark ('.', '!' and '?') is followed by a space character. (2 points)
-- marksCorrect :: String -> Bool

-- marksCorrect ""
-- marksCorrect "x"
-- marksCorrect "Hello! You look great today"
-- marksCorrect "Howdy? I'm fine. Thanks"
-- not (marksCorrect "Oh! This looks bad.Dunno why")
-- not (marksCorrect "!")
-- marksCorrect "I'm sure this goes smoothly"



-- Collect all elements with multiple occurences in a list. That is, remove elements which occur only once. (2 points)
multiple :: [Char] -> [Char]
multiple [] = []
multiple lst@(x:xs)
    | x `elem` xs   = x : multiple [ n |  n <- xs, n/= x]
    | otherwise     = multiple xs


-- multiple ""            == ""
-- multiple "haskell"     == "l"
-- multiple "erlang"      == ""
-- multiple "scala"       == "a"
-- multiple "c++"         == "+"
-- multiple "shakespeare" == "sae"




-- Define function a productOf :: (Int -> Bool) -> [Int] -> Int which calculates the product of integers that satisfy a condition. (2 points)
-- productOf (<4) [1,7,2,6,3,3,5] == 18
-- productOf even [8,32,65] == 256
-- productOf (==2) [1,2,3,2,1,2,3] == 8

productOf :: (Int -> Bool) -> [Int] -> Int
productOf p [] = 1
productOf p lst@(x:xs)
    | p x   = x * productOf p xs
    | otherwise = productOf p xs


-- Define the reverse of map. It takes an element and a list of functions. It applies each function over the element. (2 points)
reverseMap :: a -> [a -> b] -> [b]
reverseMap n (p:ps) = p n : reverseMap n ps
reverseMap n [] = []
-- reverseMap 5 [(+1), (*2), (7-)] == [6, 10, 2]
-- reverseMap 3 [\n -> take n [1..], \n -> replicate n 0, \n -> drop n [1..3]] == [[1..3], [0,0,0], []]





-- Define a function applyBoth :: (a -> b) -> (c -> d) -> (a, c) -> (b, d) which applies two functions over two components of a pair. (2 points)
-- applyBoth even (+1) (1,2) == (False,3)
-- applyBoth (== 'a') (== 'z') ('c', 'z') == (False,True)

applyBoth :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
applyBoth        p1        p2          (x,y) = (p1 x, p2 y )



-- Define a function equalsOn which checks that two functions yields same results on a list of arguments. (2 points)

-- equalsOn (^2) (*2) [2]
-- not (equalsOn (^3) (*2) [2, 87])
-- equalsOn (<100) (>50) [51..99]
-- equalsOn id (*1) [1..10]
-- not (equalsOn (div 3) (mod 2) [1..])


equalsOn :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
equalsOn             p1            p2      []  = False
equalsOn              p1           p2       last@(x:xs)
    | p1 x == p2 x       = True
    | otherwise          = equalsOn p1 p2 xs





--  Apply each function in the left component of a pair to the right component. (2 points)

elemwiseApply :: [(a -> b, a)] -> [b]
elemwiseApply [] = []
elemwiseApply ((p, n):xs) = p n : elemwiseApply xs
--     elemwiseApply [] == []
--     elemwiseApply [((*2), 0), ((+1), 3)] == [0,4]
--     elemwiseApply [(head, [1..10]), (last, [1..10])] == [1,10]





-- Replace every tab character ('\t') with a given number of spaces. (2 points)

--     Tab character is special as it is written using two characters:

--     length "\t" == 1

untabify :: Int -> String -> String
untabify m [] = []
untabify n lst@(x:xs)
    | x == '\t'      = replicate n ' ' ++ untabify n xs
    | otherwise      = x : untabify n xs
--     untabify 4 "where x = 1\t" == "where x = 1    "
--     untabify 4 "\twhere x =\t1" == "    where x =    1"
--     untabify 8 "\twhere x =\t1" == "        where x =        1"
--     untabify 4 "\t\twhere x =\t\t1" == "        where x =        1"
--     untabify 4 "" == ""






-- Extend a string to a specified width. If the string is longer than the width, do not make it shorter. (2 points)
align :: Int -> String -> String
align n [] = []
align n lst@(x:xs)
    | n > length lst      = replicate (abs(n-length lst)) ' ' ++ lst
    | n < length lst      = lst
    | otherwise           = align n xs
-- align 6 "apple"        == " apple"
-- align 7 "apple"        == "  apple"
-- align 2 "apple"        == "apple"
-- align (-8) "excellent" == "excellent"
-- align 15 "excellent"   == "      excellent"



-- Calculate the dot product of two lists, that is the sum of the products of the corresponding elements. The two lists are assumed to have the same length. (2 points)
dotProduct :: [Int] -> [Int] -> Int
dotProduct lst@(x:xs) lst1@(y:ys) = sum $ helperProduct lst lst1

helperProduct [] _ = []
helperProduct _ [] = []
helperProduct lst1@(x:xs) lst2@(y:ys) = x * y : helperProduct xs ys


-- dotProduct [1, 2] [3, 4] == 11
-- dotProduct [2, 2, 2] [5, 4, 3] == 24
-- dotProduct [3] [2] == 6
-- dotProduct [1..10] [1..10] ==  385





-- Collect elements that are sandwiched between a smaller element from the left and a larger element from the right. (2 points)
surrounded :: [Int] -> [Int]
surrounded []  = []
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

-- sameSign []
-- sameSign [-1, -12, -123]
-- sameSign [3, 7, 0, 32]
-- sameSign [0, -1]
-- sameSign [0, 0]
-- not (sameSign [-1, 1, 2, 3])
-- not (sameSign [-10..10])

sameSign :: [Int] -> Bool
sameSign lst@(x:xs) 
    | length lst == (length $ negNoHelper lst)   = True
    | otherwise                                  = False

negNoHelper lst = filter (\x -> signum x == (-1) || x == 0) lst
posNoHelper lst = filter (\x -> signum x == (1) || x == 0) lst


-- Split a list into chunks of length $n &gt; 0$. Only the last chunk is allowed to be shorter than $n$. (2 points)
runs :: [a] -> Int -> [[a]]
runs [] _ = []
runs lst@(x:xs) n  = [take n lst] ++ runs ( (drop $ length $ take n lst)  lst) n

-- runs [] 9 == []
-- runs [1,4,9,16,25,36,49,64,81,100] 3 == [[1,4,9],[16,25,36],[49,64,81],[100]]
-- runs ["apple", "plum", "peach"] 1  == [["apple"], ["plum"], ["peach"]]
-- runs ["apple", "plum", "peach", "currant", "gooseberry"] 2 == [["apple", "plum"], ["peach", "currant"], ["gooseberry"]]
-- runs [0..23] 5 == [[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14],[15,16,17,18,19],[20,21,22,23]]






-- Capitalize the first letters of all words and convert everything else to small case. (2 points)
-- titleCase :: String -> String

-- titleCase "Haskell" == "Haskell"
-- titleCase "haskeLl" == "Haskell"
-- titleCase "a tRee With APPLES" == "A Tree With Apples"
-- titleCase "a" == "A"
-- titleCase "" == ""


-- From index $i$ take $n$ elements from a list. Indexing starts with 0. We assume index $i$ and integer $n$ are nonnegative. (1 point)
-- sublist :: Int -> Int -> [a] -> [a]

-- sublist 1 4 "Haskell"  == "aske"
-- sublist 10 4 "Haskell" == ""
-- sublist 2 0 "Little Brown Fox..."   == ""
-- sublist 16 10 "Little Brown Fox..." == "..."


-- Define a function swop which replaces an element with another and vice versa. (2 points)

-- swop 'a' 'b' ""   == ""
-- swop 'a' 'b' "a"  == "b"
-- swop 'a' 'b' "b"  == "a"
-- swop 'a' 'b' "ab" == "ba"
-- swop 'H' 'W' "Haskell Wiki" == "Waskell Hiki"
-- swop 'i' 'a' "Haskell Wiki" == "Hiskell Waka"


swop :: Char -> Char -> [Char] -> [Char]
swop c1 c2 [] = []
swop c1 c2 lst@(x:xs)
    | c1 == x   = c2 : swop c1 c2 xs
    | c2 == x   = c1 : swop c1 c2 xs
    | otherwise = x : swop c1 c2 xs 




-- Collect all elements with multiple occurences in a list. That is, remove elements which occur only once. (2 points)
-- multiple :: [Char] -> [Char]

-- multiple ""            == ""
-- multiple "haskell"     == "l"
-- multiple "erlang"      == ""
-- multiple "scala"       == "a"
-- multiple "c++"         == "+"
-- multiple "shakespeare" == "sae"


-- -- Define a function `swapElems :: Eq a => a -> a -> [a] -> [a]`.
-- --   `swapElems a b xs` should replace all occurences of `a` in the list `xs`
-- --   by `b`, and all occurences of `b` by `a`.

 

-- -- Examples:
-- --   swapElems 1 2 [1]               = [2]
-- --   swapElems 1 2 [2]               = [1]
-- --   swapElems 1 2 [3]               = [3]
-- --   swapElems 1 2 [1,9,5,3,2,1,5,2] = [2,9,5,3,1,2,5,1]

 

swapElems :: Eq a => a -> a -> [a] -> [a]
swapElems n1 n2 [] = []
swapElems            n1   n2  lst@(x:xs) 
    | x == n1      = n2 : swapElems n1 n2 xs
    | x == n2      = n1 : swapElems n1 n2 xs
    | otherwise    = x : swapElems n1 n2 xs



-- -- Define a function `duplicatePred :: (a -> Bool) -> [a] -> [a]`
-- --   `duplicatePred p l` should duplicate the elements of `l` that satisfy the
-- --   predicate `p`.

-- -- Examples:
-- --   duplicatePred odd  [1, 2, 3] = [1, 1, 2, 3, 3]
-- --   duplicatePred even [1, 2, 3] = [1, 2, 2, 3]
-- --   duplicatePred odd  [2, 4, 6] = [2, 4, 6]

duplicatePred :: (a -> Bool) -> [a] -> [a]
duplicatePred p [] = []
duplicatePred          p        lst@(x:xs)
    | p x       = x : x : duplicatePred p xs
    | otherwise = x : duplicatePred p xs





-- -- Define a function 'insertAtPred'.
-- --   'insertAtPred p y xs' should insert y before the first element of xs that
-- --   satisfies the predicate p, or at the end of xs if no element of xs
-- --   satisfies the predicate p.


-- -- Examples:
-- --  insertAtPred even 99 [1, 3, 5, 2] == [1, 3, 5, 99, 2]
-- --  insertAtPred even 99 [1, 3, 5, 7] == [1, 3, 5, 7, 99]
-- --  insertAtPred odd  99 [1, 3, 5, 2] == [99, 1, 3, 5, 2]
-- --  insertAtPred even 99 []           == [99]


insertAtPred :: (a -> Bool) -> a -> [a] -> [a]
insertAtPred p n [] = [n]
insertAtPred    p n lst@(x:xs)
    | p x         = n : x : xs
    | otherwise   = x : insertAtPred p n xs




-- -- Define a function addSquaresUpTo.
-- --   'addSquaresUpTo n x' should return the list of all integers
-- --   of the form 'n + a*a' that are less than or equal to x (where a >= 0).



-- -- Examples:
-- --   addSquaresUpTo 0   48   == [0,1,4,9,16,25,36]
-- --   addSquaresUpTo 0   49   == [0,1,4,9,16,25,36,49]
-- --   addSquaresUpTo 200 100  == []
-- --   addSquaresUpTo 568 1000 == [568,569,572,577,584,593,604,617,632,649,668,689,712,737,764,793,824,857,892,929,968]

-- addSquaresUpTo :: Integer -> Integer -> [Integer] 








-- -- Define a function `f :: [(Int, a)] -> [a]`.
-- --  `f` takes a list of pairs as argument.
-- --  for each pair (x, y) in that list, `f` should add `x` times the element `y` to the result.

 

f :: [(Int, a)] -> [a]
f [] = []
f    ((n, c):xs) = replicate (abs(n)) c ++ f xs

 

-- -- Examples:
-- --  f []                             = []
-- --  f [(0, 'a')]                     = ""
-- --  f [(1, 'a')]                     = "a"
-- --  f [(2, 'a')]                     = "aa"
-- --  f [(0, 'x'), (2, 'a')]           = "aa"
-- --  f [(2, 'a'), (3, 'b'), (1, 'a')] = "aabbba"






-- -- Define a function `f :: Eq a => [a] -> [a] -> Int`.
-- -- Given two lists `xs` and `ys`, `f xs ys` should count the number of positions
-- -- where the two lists match.
-- -- Examples:
-- --  f "abc" "abc" == 3
-- --  f "abc" "aaa" == 1
-- --  f "abc" "aac" == 2
-- --  f "abc" "cab" == 0
-- --  f ""    "abc" == 0
-- -- f :: Eq a => [a] -> [a] -> Int
-- -- f = undefined





-- -- 1. Define a type Time, that can store times of the day in the format (HH AM/HH PM).
-- --   (e.g. 12 AM,  01 AM,  12 PM,  07 PM, ...)
-- -- 2. Define a function `nextTime :: Time -> Time` that advances a given time of the day by 1 hour.
-- --  nextTime 11 PM == 12 AM (or 00 AM if you want)

type HH = Int

data Time = AM HH | PM HH deriving (Show, Eq)

nextTime (AM h) = 
    case h of 
        11 -> PM 12
        _ -> AM (h+1)


nextTime (PM h) = 
    case h of 
        11 -> AM 12
        _ -> PM (h+1)



-- -- Define a function `allDifferent :: Eq a => [a] -> Bool`.
-- --   `allDifferent xs` should test whether all elements of the list xs are different.
-- -- Examples:
-- --   allDifferent []           == True
-- --   allDifferent [1, 1]       == False
-- --   allDifferent [1, 2, 3]    == True
-- --   allDifferent [1, 2, 3, 2] == False
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent lst@(x:xs)
    | x `elem` xs       = False
    | otherwise         = allDifferent xs




-- -- Define a function `fx :: Int -> [a] -> [[a]]`.
-- --`f n xs` should breaks the list `xs` into chunks of size `n`.

-- -- Examples:
-- --   fx 1 [1, 2, 3, 4] == [[1], [2], [3], [4]]
-- --   fx 2 [1, 2, 3, 4] == [[1, 2], [3, 4]]
-- --   fx 3 [1, 2, 3, 4] == [[1,2,3], [4]]
-- --   fx 4 [1, 2, 3, 4] == [[1,2,3,4]]

fx :: Int -> [a] -> [[a]]
fx n [] = []
fx     n     lst@(x:xs) = [take n lst] ++ fx n (drop (length(take n lst)) lst)



-- -- Define a function `merge3 :: Ord a => [a] -> [a] -> [a] -> [a]`. `merge3`
-- --   takes 3 sorted list `xs`, `ys`, `zs` and returns a single sorted list
-- --   containing the elements of `xs`, `ys` and `zs`. (without using the sort
-- --   function).


-- -- Examples:
--   -- merge3 [1, 2, 3] [7, 8, 9] [4, 5, 6] = [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   -- merge3 [1, 4, 9] [2, 6, 8] [3, 5, 7] = [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   -- merge3 [1, 2, 6] [3, 4, 5] []        = [1, 2, 3, 4, 5, 6]
--   -- merge3 []        [1, 2, 3] []        = [1, 2, 3]

myMerge _ _ [] = []
myMerge _ [] _ = []
myMerge [] _ _ = []

myMerge lst1@(x:xs) lst2@(y:ys) lst3@(z:zs)
    | x <= y && x <= z   = x : myMerge xs ys zs
    | y <= x && y <= z   = y : myMerge xs ys zs
    | z <= x && z <= y   = z : myMerge xs ys zs 

merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 lst1 lst2 lst3 = mySort (lst1 ++  lst2 ++ lst3)


mySort [] = []
mySort lst@(x:xs) = insert x (mySort xs)

myInsert n [] = [n]
myInsert n lst@(x:xs)
    | n < x   = n:x:xs
    | otherwise = x : myInsert n xs




-- -- Define a function `f :: (a -> Bool) -> [a] -> [[a]]`.
-- --   `f p xs` should be the list of the maximal continuous sublists of `xs` that
-- --   consist only of elements satisfying the predicate `p`.


-- -- Examples:
-- --   f even [0, 1, 2, 3] == [[0], [2]]
-- --   f odd  [0, 1, 2, 3] == [[1], [3]]
-- --   f even [0, 2, 1, 3] == [[0, 2]]
-- --   f odd  [0, 2, 1, 3] == [[1, 3]]
-- --   f odd  [0, 2, 4]    == []
-- --   f odd  [1, 1, 1, 0, 1, 1, 1] == [[1, 1, 1], [1, 1, 1]]


-- f :: (a -> Bool) -> [a] -> [[a]]
-- f = undefined







-- -- Define a higher-order function `f :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]`
-- --   that uses all of its arguments.


ff :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]
ff            f1                f2           lst  = [swap $ f1 ((head lst), f2 (head lst) (head lst))]



-- Generate a list using a generator function and an initial element. The generation process continues until the generator function returns Nothing. If it returns Just x then x goes into the result list. (2 points)

--     The initial element is the first in the result list.

--     Note: Function generate is very similar to iterate.

generate :: (a -> Maybe a) -> a -> [a]
generate           p          x = 
    case (p x) of
        Nothing -> [x]
        (Just y) -> 

--     generate (\n -> if n < 5 then Just (n+1) else Nothing) 0 == [0..5]
--     generate (\n -> if n < 5 then Just (n+1) else Nothing) 3 == [3..5]
--     generate (\n -> if n < 5 then Just (n+1) else Nothing) 7 == [7]
--     generate (\n -> if n < 9 then Just (2*n) else Nothing) 1 == [1,2,4,8,16]
--     generate (\_ -> Nothing) 0   == [0]
--     generate (\_ -> Nothing) 7   == [7]
--     generate (\_ -> Nothing) 'a' == ['a']
--     generate (\_ -> Nothing) "a" == ["a"]




-- Modify the first element in a list with a function f, which returns Maybe. 
-- If f returns Nothing, remove the first element. If f returns Just x, replace the first element with x.

-- modify (\x -> Nothing) [1..10]          == [2..10]
-- modify (\x -> Just 20) [1,2,3]          == [20,2,3]
-- modify (\x -> Just (toUpper x)) "apple" == "Apple"
-- modify (\x -> Just (toUpper x)) ""      == ""

modify :: (a -> Maybe a) -> [a] -> [a]
modify p [] = []
modify           p       lst@(x:xs) = 
    case (p x) of
        Nothing -> xs
        Just y  -> y : xs





getFirst lst = map (\x -> (head x)) $ words lst -- ++ ". " -- ++  (map (\x -> (head x)) $ words lst)
monogram lst =
    case  getFirst lst of
        xxs -> breakList xxs


breakList [] = []
breakList lst@(x:xs)
    | isUpper x = x : '.' : ' ' : breakList xs
    | otherwise = breakList xs
