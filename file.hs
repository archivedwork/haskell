import Data.Char
import Data.List


mapInc :: [Int] -> [Int]
mapInc (x:xs) = (x+1) : mapInc xs
mapInc [] = []


filterEven :: [Int] -> [Int]
filterEven (x:xs)
    | even x = x : filterEven xs
    | otherwise = filterEven xs
filterEven [] = []


-- [x..y]
countTo a b
    | a > b = []
    | otherwise = a : countTo (a+1) b



elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  --  | e == x = True
    | all (\x -> x == e) (x:xs) = True
    | otherwise =  elem' e xs



intersection xs ys = filter (\x -> x `elem` ys) xs


hasAny xs ys = not $ null $ filter (\x -> x `elem` ys) xs

 -- Define a function `f :: [Int] -> [Int]`.
--   The list `f xs` should be a sublist of `xs`. 
--   An element `x` of `xs` is included in `f xs` if it is 
--   greater than every element `y` that occurs later in the list `xs`.
-- Examples:
--   f [3,2,1]             == [3,2,1]
--   f [1,2,3]             == [3]
--   f [3,2,1,3,2,1]       == [3,3,2,1]
--   f [1,2,3,2,1]         == [3]
--
--   f [4,9,1,8,3,6,7,3,2] == [9,8,7,3,2]


greaterThan a  [] = True
greaterThan a (x:xs)
    | a >= x     =  True -- x : greaterThan a xs
    | otherwise = greaterThan a xs



f :: [Int] -> [Int]
f (x:xs)
    -- | greaterThan x xs = x : f xs
    | x > maximum (x:xs) = x :  f xs
    | otherwise = f xs
f [] = []


-- map (+1) [1,4,8]
map1 f (x:xs) = f x : map1 f xs
map1 f [] = []

intersect1 lst1 lst2 = filter (\x -> x `elem` lst2) lst1

zeroToN :: Int -> [Int]
zeroToN 0 = []
zeroToN n =  n : zeroToN (n-1)
zzToNN n = reverse $ zeroToN n

-- or zeroToN n = [0..n]


ping f x 
    | f x == True = "Pong"
    | otherwise = "Pang"


--- isSmile :: Ord a => [a] -> Bool
isSmile "=>" = False
isSmile ";/" = False
isSmile [] = False
isSmile (x:y:xs)
    | x == ':' || x == '=' || x == ';' && y == ')' || y == ']' || y == '>' || y == 'D' || y == '/' || y == '|' || y == '}' = True
    | otherwise = False



manyInts a1 a2 a3 a4 a5 a6 a7 = [x | x <- [a1,a2,a3,a4,a5,a6,a7], x `mod` 3 == 0]



disjoint lst1 lst2 = null $ filter (\x -> x `elem` lst2) lst1


equalsOn f1 f2 [] = False
equalsOn f1 f2 (x:xs)   -- -> bool
    | f1 x == f2 x = True
    | otherwise = equalsOn f1 f2 xs


applyBoth f1 f2 (x,y) = (f1 x, f2 y)


productOf f (x:xs)
    | f x = x * productOf f xs
    | otherwise = productOf f xs
productOf f [] = 1


ff :: Int -> [Int] -> [Int]
-- f 3 [1,2,3,4] === [1,1,1,2,2,2,3,3,3,4,4,4]
ff n arr@(x:xs) = replicate n x ++ ff (n) xs
ff n [] = []

hello_worlds 1 = ""
hello_worlds n = "Hello World" ++ hello_worlds (n-1)

filtr f (x:xs)
  | f x             = x : filtr f xs
  | otherwise  =  filtr f xs 
filtr f [] = []


f1 :: Int -> [Int] -> [Int]
f1 n (x:xs) =  filtr (<n) (x:xs) 



f2 :: [Int] -> [Int]
f2  lst@(x:xs)      = [v | (v,i) <- zip lst [1..], i `mod` 2 /= 0]



apply :: (a, a -> b -> c, b) -> c
apply (x, op, y) = x `op` y



isVowel :: Char -> Bool
isVowel chr
    | chr `elem` "aeiou" = True
    | otherwise = False


mapBoth :: (a -> b) -> (a -> c) -> a -> (b,c)
mapBoth f1 f2 x = (f1 x, f2 x)

liftMaybe :: (Maybe a, Maybe b) -> Maybe (a,b)
liftMaybe (Nothing, Just _) = Nothing
liftMaybe (Just _, Nothing) = Nothing
liftMaybe (Nothing, Nothing) = Nothing
liftMaybe (Just x, Just y) = Just (x,y)


elemwiseApply :: [(a->b, a)] -> [b]
elemwiseApply [] = []
elemwiseApply ((f,x):xs) = (f x) : elemwiseApply xs


factors n = [x | x <- [1..n], n `mod` x ==  0]

isPrime x = factors x == [1,x]

primeNumber x y = [a | a <- [x..y], isPrime a]


expand :: [(Int, a)] -> [a]
expand ((n, x):xs) = (replicate n x) ++ expand xs
expand [] = []


oddIx lst = [i | (v,i) <- zip lst [0..], v `mod` 2 /= 0]


addIndex lst = [v+i | (v,i) <- zip lst [0..]]


-- generate :: (a -> Maybe a) -> a -> [a]
-- -- generate (\n -> if n < 5 then Just (n+1) else Nothing) 0 == [0..5]
-- generate f n
--     | f == Just (n) = n : generate f (n+1)
--     | otherwise = Nothing
-- generate Nothing x = [x]


data Number = Even Int | Odd Int deriving (Eq, Show)

inc :: Number -> Number
inc (Even x) = Odd (x+1)
inc (Odd x) = Even (x+1)



range :: Number ->  Number -> [Number]
range (Even x) (Odd y) 
    | x > y = []
    | otherwise = [Even x] ++ range (Even (x+1)) (Odd y)

range (Odd x) (Even y) 
    | x > y = []
    | otherwise = [Odd x] ++ range (Odd (x+1)) (Even y)
range (Even x) (Even y) = [Even x]
range (Odd x) (Odd y) = [Odd y]



pipeline [f1, f2, f3] x = f1 $ f2 $ f3 x 


-- anyExit :: (a -> b -> Bool) -> [a] -> [b] -> [a]
anyExit p (x:xs) (y:ys) 
    -- | p x y = x : anyExit p xs ys
    -- | otherwise = anyExit p xs ys
 = filter (\a -> a `elem` (x:xs)) (y:ys)
anyExit p [] _ = []
anyExit p _ [] = []




startEnd [x] = [x]
startEnd [] = []
startEnd lst = [head lst] ++ [last lst]


sublist x y lst
    | x <= length(lst) || y <= length(lst) = take y (drop x lst)
    | otherwise = []


exchange n =  n * 0.89

rep_mul 0 y = []
rep_mul x y = y : rep_mul (x-1) y


mul x y = sum $ rep_mul x y

mull x 1 = x
mull x y = x + mull x (y-1)



wordsWithCap_helper (x:xs) acc
    | isUpper (x)     = acc ++ x : wordsWithCap_helper (xs) acc
    | otherwise = wordsWithCap_helper xs acc

wordsWithCap_helper lst acc =  acc


wordsWithCapital lst = wordsWithCap_helper lst []






-- Define a higher-order function `f :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]`
--   that uses all of its arguments.


-- fp :: ((a, b) -> (b, c)) -> (a -> a -> b) -> [a] -> [(c, b)]
-- -- fp          f1                 f2             lst = [(c,b)]              
-- fp f1 f2 lst = [swap $ f1 (head lst, f2 (head lst) (head lst))]

-- -- [swap $ f1 (head lst, f2 (head lst) (head lst)) . f2 (head lst) (head lst) ]


-- -- f1_example (x,y) = (y, True)

