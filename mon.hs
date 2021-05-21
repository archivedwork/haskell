import Data.List
import Data.Char




data Day = MON | TUE | WED | THR | FRI | SAT | SUN  deriving (Enum, Show, Eq)

isFirstDayOfWeek :: Day -> Bool
isFirstDayOfWeek MON = True
isFirstDayOfWeek _ = False


next :: Day -> Day
next d = (succ d)



type Hour = Int
type Minutes = Int

data Time = T Hour Minutes deriving (Show, Eq)

showTime :: Time -> String
showTime (T h m) = show h ++ "." ++ show m


eqTime (T h1 m1) (T h2 m2)
    | h1 == h2 && m1 == m2   = True
    | otherwise  = False


isEarlier (T h1 m1) (T h2 m2)
    | h1 == h2 || h1 > h2 && m1 == m2 = False
    | h1 < h2 && m1 > m2 || h1 == h2 && m1 < m2   = True
    -- | otherwise = True

-- test_isEarlier = [
-- isEarlier (T 10 15) (T 10 30) == True,
-- isEarlier (T 10 35) (T 12 30) == True,
-- not (isEarlier (T 12 30) (T 12 30)) == True,
-- not (isEarlier (T 10 35) (T 10 30)) == True,
-- not (isEarlier (T 13 30) (T 12 30)) == True]


isBetween (T h1 m1) (T h2 m2) (T h3 m3)
    | (h1 < h2 && h2 < h3) || (m1 < m2 && m2 < m3)= True
    | (h3 < h2 && h2 < h1) || (m3 < m2 && m2 < m1)= True
    | otherwise = False


-- time h m 
-- | h `elem` [1..12]  = 
-- = check_hour h ++ check_minutes m

check_hour h =
    case h of 
        13 ->  "time: invalid hour: 13"
        24 ->  "time: invalid hour: 24"


check_minutes m = 
    case m of 
        60 -> "time: invalid minute: 60"



data USTime = AM Int Int | PM Int Int deriving (Show, Eq)


showUSTime (AM h m) = show h ++ "." ++ show m ++ "am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ "pm"


usTimeToTime (AM h1 m1) (T h2 m2)
    | h1 `elem` [1..12] && h2 `elem` [1..12] = True
    | h1 `elem` [1..12] && h2 == 0   = True
    | h1 `elem` [1..12] && h2 `elem` [13..23] = True


timeToUSTime (T h m) = 
    case h of 
        00 -> AM 12 m
        13 -> PM 1 m
        14 -> PM 2 m
        15 -> PM 3 m 
        16 -> PM 4 m 
        17 -> PM 5 m 
        18 -> PM 6 m 
        19 -> PM 7 m 
        20 -> PM 8 m 
        21 -> PM 9 m 
        22 -> PM 10 m 
        23 -> PM 11 m 
        h -> AM h m



------------------------------- 6-6-2019 ------------------------

apply :: (a, a -> b -> c, b) -> c
apply (x, op, y) = op x y


-- test_apply= [
-- apply (2, (+), 1)== 3,
-- apply (2, (-), 1 )   == 1,
-- apply (1, (:), [])   == [1],
-- apply ([1..3], (++), [4..6]) == [1..6]]

-- Check whether a character is a small vowel in the English alphabet. (1 point)

isVowel :: Char -> Bool
isVowel c 
    | c `elem` "aeiou" = True
    | otherwise = False

-- test_isVowel = [
-- isVowel 'a' == True,
-- isVowel 'b' == False,
-- isVowel 'c' == False,
-- isVowel 'd' == False,
-- isVowel 'e' == True]

-- Call two functions on the same parameter and collect both results. (1 point)

mapBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
mapBoth  f1 f2 x = (f1 x, f2 x)

-- test_mapBoth = [
--  mapBoth (*2) (+1) 0   == (0,1),
--  mapBoth fst snd (0,'a')   == (0,'a'),
--  mapBoth head last [1..10] == (1,10)]



-- Extract two values from two Maybes. If one of the Maybes is a Nothing then return a Nothing. Otherwise, return both values in a pair in a Maybe. (1 point)

liftMaybe :: Eq a => (Maybe a, Maybe b) -> Maybe (a,b)
liftMaybe (Nothing, Just x)   = Nothing
liftMaybe (Just x,  Nothing)  = Nothing
liftMaybe (Nothing, Nothing)  = Nothing
liftMaybe (Just x,  Just y) = Just (x,y)


-- test_liftMaybe = [
-- liftMaybe (Nothing, Just 0)   == Nothing,
-- liftMaybe (Just 0,  Nothing)  == Nothing,
-- liftMaybe (Nothing, Nothing)  == Nothing,
-- liftMaybe (Just 'a',  Just 1) == Just ('a',1)


-- Apply each function in the left component of a pair to the right component. (2 points)

elemwiseApply :: [(a -> b, a)] -> [b]
elemwiseApply [] = []
elemwiseApply ((op, x):xs) = op x : elemwiseApply xs

-- elemwiseApply [] == []
-- elemwiseApply [((*2), 0), ((+1), 3)] == [0,4]
-- elemwiseApply [(head, [1..10]), (last, [1..10])] == [1,10]




-- Replace every tab character ('\t') with a given number of spaces. (2 points)

-- Tab character is special as it is written using two characters:

-- length "\t" == 1

untabify :: Int -> String -> String
untabify n [] = []
untabify n lst@(x:xs) 
    | x == '\t'   = take n (repeat ' ') ++ untabify n xs
    | otherwise = x : untabify n xs

test_untabify = [
    untabify 4 "where x = 1\t" == "where x = 1",
    untabify 4 "\twhere x =\t1" == "where x =1",
    untabify 8 "\twhere x =\t1" == "where x =1",
    untabify 4 "\t\twhere x =\t\t1" == "where x =1",
    untabify 4 "" == ""]

-- Enumerate prime numbers in an interval. The interval is closed on both ends, meaning that lower bound and upper bound should be in the result list if they are prime numbers. (2 points)

-- primeNumbers :: Int -> Int -> [Int]

-- primeNumbers 2 10 == [2,3,5,7]
-- primeNumbers 7 23 == [7,11,13,17,19,23]
-- primeNumbers 5 5  == [5]
-- primeNumbers 4 4  == []
-- primeNumbers 9 10  == []

-- Expand each pair in a list so that an element e occurs n times in the result. (2 points)

expand :: [(Int, a)] -> [a]
expand [] = []
expand ((n, c):xs) = replicate n c ++ expand xs

-- test_expand = [
-- expand [(1,'a'), (0,'b'), (2,'c')] == "acc",
-- expand [(0,'a'), (1,'b'), (2,'c')] == "bcc",
-- expand [(1,'a'), (2,'b'), (0,'c')] == "abb",
-- expand [(0,True), (0,False)] == [],
-- expand [] == []]

-- Enumerate indices of odd numbers in a list. (2 points)

oddIx :: [Int] -> [Int]
oddIx [] = []
oddIx lst@(x:xs) = [i | (v,i) <- zip lst [0..], v `mod` 2 /= 0]
-- oddIx [1]   == [0]
-- oddIx [1,2] == [0]
-- oddIx []== []
-- oddIx [0..10] == [1,3,5,7,9]
-- oddIx [1..10] == [0,2,4,6,8]
-- oddIx [1,4,6,7,3,14,8] == [0,3,4]



-- Add the index of each element to the element itself. (2 points)

addIndex :: [Int] -> [Int]
addIndex lst@(x:xs) = [v+i | (v,i) <- zip lst [0..]]
addIndex [] = []

-- addIndex []  == []
-- addIndex [0] == [0]
-- addIndex [0,0,0,0] == [0..3]
-- addIndex [0..5]== [0,2..10]
-- addIndex [1,5,4]   == [1,6,6]

-- Generate a list using a generator function and an initial element. The generation process continues until the generator function returns Nothing. If it returns Just x then x goes into the result list. (2 points)

-- The initial element is the first in the result list.

-- Note: Function generate is very similar to iterate.

generate :: (a -> Maybe a) -> a -> [a]
generate p n = 
    case p n of 
        Nothing -> [n]
        (Just (x)) ->  n : generate p (x)

-- generate (\n -> if n < 5 then Just (n+1) else Nothing) 0 == [0..5]
-- generate (\n -> if n < 5 then Just (n+1) else Nothing) 3 == [3..5]
-- generate (\n -> if n < 5 then Just (n+1) else Nothing) 7 == [7]
-- generate (\n -> if n < 9 then Just (2*n) else Nothing) 1 == [1,2,4,8,16]
-- generate (\_ -> Nothing) 0   == [0]
-- generate (\_ -> Nothing) 7   == [7]
-- generate (\_ -> Nothing) 'a' == ['a']
-- generate (\_ -> Nothing) "a" == ["a"]



-- Consider the following type for numbers. It distinguishes odd and even numbers.

data Number = Even Int | Odd  Int  deriving (Eq, Show)

-- Define a function which increases a Number. (1 point)

inc :: Number -> Number
inc (Even x) = Odd (x+1)
inc (Odd x) = Even (x+1)

-- inc (Even 0) == Odd 1
-- inc (Even 8) == Odd 9
-- inc (Odd 1)  == Even 2
-- inc (Odd 7)  == Even 8

 


-- Create an interval of Numbers. The interval is closed on both ends, meaning both ends should be in the list. (2 points)

-- The first parameter may be greater than the second. In that case, the result should be empty.

range :: Number -> Number -> [Number]
range (Even x) (Odd y) | x > y = []
range (Even x) (Odd y) | x == y = [Even x]
range (Odd x) (Odd y) | x == y = [Odd x]
range (Even x) (Even y) | x == y = [Even x]
range (Even x) (Odd y) = map (\x -> if even x then Even x else Odd x) [x..y]
range (Odd x) (Even y) = map (\x -> if odd  x then Odd x else Even x) [x..y]




-- range (Even 0) (Odd 3)  == [Even 0, Odd 1, Even 2, Odd 3]
-- range (Odd 5) (Even 8)  == [Odd 5, Even 6, Odd 7, Even 8]
-- range (Odd 5) (Odd 5)  == [Odd 5]
-- range (Even 4) (Even 4) == [Even 4]
-- range (Even 4) (Odd 3)  == []



-- Define function composition of a list of functions. (3 points)

-- pipeline [f1,f2, ... , fn]   = f1 . f2 . ... . fn
-- pipeline [f1,f2, ... , fn] x = f1 (f2 (... (fn x)))

pipeline :: [a -> a] -> (a -> a)

pipeline [op1, op2, op3] n = (op1 . op2 .op3) n


-- pipeline [(*2), (+1), (*3)] 1 == 8            -- sum [2, 2, 3]
-- pipeline [(+1), (*2), (*3)] 1 == 7
-- pipeline [(*2), (*3), (+1)] 1 == 12
-- pipeline [map (*2), take 3, drop 2] [1..10] == [6,8,10]




-- Given a predicate (p) and two lists (xs and ys). Enumerate those x elements in xs for which exists an y in ys such that the predicate holds. (3 points)

-- Formally, we collect x elements for which ∃ y ∈ ys such that p x y is true.

anyExists :: (a -> b -> Bool) -> [a] -> [b] -> [a]
anyExists p [] _ = []
anyExists p _ [] = []
anyExists p lst1@(x:xs) [y] | p x y = x : anyExists  p xs [y]
anyExists p lst1@(x:xs) [y,z] | p x y = x : anyExists p xs [z]
anyExists p lst1@(x:xs) lst2@(y:ys)
    | p x y      =  x : anyExists p xs ys
    | otherwise  = anyExists p xs ys




-- anyExists (<) [1..10] []== []
-- anyExists (<) [] [10..20]   == []
-- anyExists (<) [1..10] [4]   == [1..3]
-- anyExists (<) [1..10] [7]   == [1..6]
-- anyExists (<) [1..10] [4,7] == [1..6]
-- anyExists (\x y -> x + y == 5) [1..10] [2,4] == [1,3]
-- anyExists (\x y -> x - y == 5) [1..10] [2,4] == [7,9]

-- Check that list zs can be created by interleaving lists xs and ys. That is, elements in zs come from either xs or ys in the same order they are in xs and ys. (3 points)

-- The interleaving process does not repeat elements in zs.

-- split :: Eq a => [a] -> [a] -> [a] -> Bool

-- split [] [] []  == True
-- split [1] [] [1]== True
-- split [] [1] [1]== True
-- split [1] [1] []== False
-- split [1] [2] [1,2] == True
-- split [1] [2] [2,1] == True
-- split [1] [1,2] [1,1,2] == True
-- split [1] [1,2] [1,2,1] == True
-- split [1] [1,2] [2,1,1] == False
-- split [1,2,3] [4,5,6] [1,2,4,5,3,6] == True
-- split [1,2,3] [4,5,6] [4,1,2,5,3,6] == True
-- split [1,2,3] [4,5,6] [4,1,3,5,2,6] == False





-- startsWith "Ha" "Haskell"
-- startsWith "Scal" "Scala"
-- startsWith "" "a"
-- startsWith "" ""
-- startsWith "Real World" "Real World Haskell"
-- not (startsWith "a" "")
-- not (startsWith "ha" "Haskell")

startsWith [] _ = True
startsWith _ [] = False
startsWith lst1@(x:xs) lst2@(y:ys)
    | x == y   = startsWith xs ys
    | otherwise = False



surrounded :: [Int] -> [Int]
surrounded [] = []
surrounded [x] = []
surrounded [x,y] = [] 
surrounded lst@(x:y:z:xs)
    | y > x && y < z   = [y] ++ surrounded (y:z:xs)
    | otherwise        = surrounded (y:z:xs) 


-- surrounded [1, 3, 5]     == [3]
-- surrounded [2, 4, 8, 10] == [4, 8]
-- surrounded [4, 3, 5, 9]  == [5]
-- surrounded [1]   == []
-- surrounded [1,5] == []
-- surrounded []    == []