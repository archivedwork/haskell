import Data.List
import Data.Char



-- Return the first and the last element of a list. (1 point)
startEnd :: [Int] -> [Int]
startEnd [] = []
startEnd [x] = [x]
startEnd lst@(x:xs) = [head lst, last lst]


-- startEnd []          == []
-- startEnd [10]        == [10]
-- startEnd [1,10]      == [1,10]
-- startEnd [100,99..5] == [100,5]



-- From index $i$ take $n$ elements from a list. Indexing starts with 0. We assume index $i$ and integer $n$ are nonnegative. (1 point)
sublist :: Int -> Int -> [a] -> [a]
sublist i n lst@(x:xs)
    | i >= 0 && n >= 0 || i <= length lst || n <= length lst = take n $ drop i lst
    | otherwise = []


-- sublist 1 4 "Haskell"  == "aske"
-- sublist 10 4 "Haskell" == ""
-- sublist 2 0 "Little Brown Fox..."   == ""
-- sublist 16 10 "Little Brown Fox..." == "..."


-- Mix elements of a triple conforming to the type. (1 points)
-- shuffle :: (a, b, c) -> (b, c, a)

-- shuffle (1, True, '?')            == (True, '?', 1)
-- shuffle ("Haskell", '!', [5,6,7]) == ('!', [5,6,7], "Haskell")

-- Exchange US dollars to euros. 1 US dollar is worth 0.89 euros. (1 point)
exchange :: Int -> Double
exchange x = fromIntegral x * 0.89


-- exchange 1000 == 890.0
-- exchange 50   == 44.5
-- exchange 75   == 66.75


-- Define integer multiplication recursively with addition and subtraction only. (2 points)
mul :: Integer -> Integer -> Integer
mul x  0 = 0
mul 0 y = 0
mul x y = x + mul x (y-1)
-- mul 0 2   == 0
-- mul 5 0   == 0
-- mul 5 6   == 30
-- mul 10 12 == 120




-- Modify the first element in a list with a function f, which returns Maybe. If f returns Nothing, remove the first element. If f returns Just x, replace the first element with x.
-- modify (\x -> Nothing) [1..10]          == [2..10]
-- modify (\x -> Just 20) [1,2,3]          == [20,2,3]
-- modify (\x -> Just (toUpper x)) "apple" == "Apple"
-- modify (\x -> Just (toUpper x)) ""      == ""

modify :: (a -> Maybe a) -> [a] -> [a]
modify f [] = []
modify f (x:xs) = 
    case f x of
        Nothing -> xs
        Just y  -> (y : xs)



-- Count words starting with upper case letters. (2 points)
wordWithCapital :: String -> Int
wordWithCapital lst  = length $ filter (\x -> isUpper (head x)) $ words lst

-- wordWithCapital "Every Word Starts With Upper Case Letter" == 7
-- wordWithCapital "Here not Every one Starts With capital"   == 4
-- wordWithCapital "thiS hAs SoME weiRd words As weLL"        == 2



-- Define a function minList which traverses two lists simultaneously and takes the smaller element at each position. (2 points)
minList :: [Int] -> [Int] -> [Int]
minList _ [] = []
minList [] _ = []
minList lst1@(x:xs) lst2@(y:ys)
    | x <= y      = x : minList xs ys
    | otherwise  =  y : minList xs ys

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




-- Calculate the average of a nonempty list of rational numbers (1 point)
avg :: [Double] -> Double
avg lst@(x:xs) = (summ lst) / fromIntegral (length lst)


summ [] = 0 
summ lst@(x:xs) = x + summ xs

-- avg [2.2, 4.4, 3.5, 5.9] == 4.0
-- avg [2.5] == 2.5





-- Define a function swop which replaces an element with another and vice versa. (2 points)
-- swop :: Char -> Char -> [Char] -> [Char]

-- swop 'a' 'b' ""   == ""
-- swop 'a' 'b' "a"  == "b"
-- swop 'a' 'b' "b"  == "a"
-- swop 'a' 'b' "ab" == "ba"
-- swop 'H' 'W' "Haskell Wiki" == "Waskell Hiki"
-- swop 'i' 'a' "Haskell Wiki" == "Hiskell Waka"
-- Collect all elements with multiple occurences in a list. That is, remove elements which occur only once. (2 points)
-- multiple :: [Char] -> [Char]

-- multiple ""            == ""
-- multiple "haskell"     == "l"
-- multiple "erlang"      == ""
-- multiple "scala"       == "a"
-- multiple "c++"         == "+"
-- multiple "shakespeare" == "sae"





-- 1. Define a type Time, that can store times of the day in the format (HH AM/HH PM).
--   (e.g. 12 AM,  01 AM,  12 PM,  07 PM, ...)
-- 2. Define a function `nextTime :: Time -> Time` that advances a given time of the day by 1 hour.
--  nextTime 11 PM == 12 AM (or 00 AM if you want)

type HH = Int
data Time = AM HH | PM HH  deriving (Show, Eq)


nextTime :: Time -> Time 
nextTime (PM h) =
    case h of 
        11 -> AM 12
        _ -> AM h

nextTime (AM h) = 
    case h of 
        11 -> PM 12
        _ -> AM (h+1)


-- sort (rook (0,0)) == [(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]
-- sort (rook (7,7)) == [(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6)]
rook :: (Int, Int) -> [(Int, Int)]
rook (x,y) = [(a,y) | a <- [0..6]] ++ [(x,a) | a <- [0..6]]


-- Capitalize the first letters of all words and convert everything else to small case. (2 points)
titleCase :: String -> String
titleCase [] = []
titleCase lst@(x:xs) = mapToUpper lst ++ mapToLower lst



titleCase2 lst = unwords $ [toUpper x : map (\x -> toLower x) xxs |  xs <- words lst, x <- [head xs], xxs <- [tail xs]]


mapToLower lst@(x:xs) = map (\x -> toLower x) $ tail lst

mapToUpper lst = map (\x -> toUpper x) [head lst]


-- map (\x -> toUpper x)  [head xs] ++ (map (\x -> toLower x) $  tail xs)

-- titleCase "Haskell" == "Haskell"
-- titleCase "haskeLl" == "Haskell"
-- titleCase "a tRee With APPLES" == "A Tree With Apples"
-- titleCase "a" == "A"
-- titleCase "" == ""

-- count a [] = 0
-- count a lst@(x:xs)
--     | a == x      = 1 + count a xs
--     | otherwise   = count a xs

-- f4 [] = []
-- f4 lst@(x:xs) = [count x xs] ++ f4 xs

-- f lst = reverse $ f4 lst 

-- f4Count ::  Eq a =>a-> [a] -> Int
-- f4Count _ []=0
-- f4Count a (x:xs)
--  |a ==x=1+f4Count a xs
--  |otherwise =f4Count a xs

-- f4Helper :: Eq a => [a] -> [Int]
-- f4Helper []=[]
-- f4Helper (x:xs)=[f4Count x xs]++f4Helper xs


-- f4 :: Eq a => [a] -> [Int]
-- f4 []=[]
-- f4 [x]=[0]
-- f4 (x:xs)=reverse (f4Helper (reverse (x:xs)))


--   f [1, 1, 1, 1]       = [0, 1, 2, 3]
--   f [1, 2, 3, 4]       = [0, 0, 0, 0]
--   f [1, 2, 1, 2, 2, 1] = [0, 0, 1, 1, 2, 2]
--   f []                 = []


fx n [] = []
fx n lst@(x:xs) = [take n lst] ++ fx n xs
-- Examples:
--   fx 1 [1, 2, 3, 4] == [[1], [2], [3], [4]]
--   fx 2 [1, 2, 3, 4] == [[1, 2], [3, 4]]
--   fx 3 [1, 2, 3, 4] == [[1,2,3], [4]]
--   fx 4 [1, 2, 3, 4] == [[1,2,3,4]]



-- Define a function join which inserts a list between every two lists and concatenates all the lists. (2 points)
-- Do not use intercalate or intersperse.
-- join :: [a] -> [[a]] -> [a]
join str [] = []
join str (lst2@(a:as):ys) 
    | ys == []   = lst2 ++ join str ys
    | otherwise  = lst2 ++  str ++  join str (ys)

-- join " " ["Haskell", "is", "wonderful"] == "Haskell is wonderful"
-- join [0] [[1], [2], [3]] == [1,0,2,0,3]
-- join "." ["O", "o"] == "O.o"
-- join "a" (replicate 5 "N") == "NaNaNaNaN"