import Data.List
import Data.Char


expand :: [(Int, a)] -> [a]
expand lst@((n, e):xs) = expandHelper lst []

-- tail recursion
expandHelper [] acc             =  acc
expandHelper lst@((n,e):xs) acc =  replicate n e ++ expandHelper xs acc ++ acc


test_expand = [
    expand [(1,'a'), (0,'b'), (2,'c')] == "acc",
    expand [(0,'a'), (1,'b'), (2,'c')] == "bcc",
    expand [(1,'a'), (2,'b'), (0,'c')] == "abb",
    expand [(0,True), (0,False)] == []]
   -- expand [] == [] ]


anyExists :: (a -> b -> Bool) -> [a] -> [b] -> [a]
anyExists p lst1 lst2 = anyExistsHelper p lst1 lst2 []


anyExistsHelper p lst1 []  acc = acc
anyExistsHelper p [] lst2  acc = acc
anyExistsHelper p lst1@(a:as) lst2@(b:bs) acc
    | p a b         = anyExistsHelper p as lst2 acc
    | otherwise     = acc ++ [a] ++ anyExistsHelper p as bs acc 

test_anyExits = [
    anyExists (<) [1..10] []    == [],
    anyExists (<) [] [10..20]   == [],
    anyExists (<) [1..10] [4]   == [1..3],
    anyExists (<) [1..10] [7]   == [1..6],
    anyExists (\x y -> x <  y) [1..10] [4,7] == [1..6],

    anyExists (\x y -> x + y == 5) [1..10] [2,4] == [1,3],
    anyExists (\x y -> x - y == 5) [1..10] [2,4] == [7,9]]


anyEx p [] lst2 = []
anyEx p lst1 [] = []
anyEx p lst1@(x:xs) lst2@(y:ys)
    | p x y = x : anyEx p xs lst2
    | otherwise =  anyEx p xs lst2





decompress :: String -> String
-- x == 'a'
-- y == 5
decompress [] = []
decompress lst@(c:n:cs) = replicate (digitToInt n) c ++ decompress (cs) 




test_decompress =
  [ decompress "a5b4c2" == "aaaaabbbbcc"
  , decompress "d9d6" == "ddddddddddddddd"
  , decompress "a2" == "aa"
  , decompress "a1b1c1" == "abc"
  , decompress "" == ""
  ]

-- Ex: return a list of length 1000 with False and True alternating in it: [False, True, False, True, False, ...]

falseTrues :: [Bool]
falseTrues = falseTruesHelper 1000

falseTruesHelper :: Int -> [Bool]
falseTruesHelper 0 = []
falseTruesHelper n = False : True : falseTruesHelper (n-1)




-- Define a function addSquaresUpTo.
--   'addSquaresUpTo n x' should return the list of all integers
--   of the form 'n + a*a' that are less than or equal to x (where a >= 0).



-- Examples:

test_addSquare = [
  addSquaresUpTo 0   48   == [0,1,4,9,16,25,36],
  addSquaresUpTo 0   49   == [0,1,4,9,16,25,36,49],
  addSquaresUpTo 200 100  == [],
  addSquaresUpTo 568 1000 == [568,569,572,577,584,593,604,617,632,649,668,689,712,737,764,793,824,857,892,929,968]]



addSquaresUpTo n x = [n+a*a | a <- [0..x], a >= 0, (n+a*a) <= x]



-- Examples:
test_swapElem = [
  swapElems 1 2 [1]               == [2],
  swapElems 1 2 [2]               == [1],
  swapElems 1 2 [3]               == [3],
  swapElems 1 2 [1,9,5,3,2,1,5,2] == [2,9,5,3,1,2,5,1]]

 

swapElems :: Eq a => a -> a -> [a] -> [a]
swapElems n1 n2 [] = []
swapElems n1 n2 lst@(x:xs)
    | n1 == x    =  n2 : swapElems n1 n2 xs
    | n2 == x    = n1 : swapElems n1 n2 xs
    | otherwise  = x : swapElems n1 n2 xs




-- function composition 
-- dropSpaces s = dropWhile isSpace s

--trim s = (reverse . dropSpaces) s

-- or

--trim = reverse . dropSpaces


--reverseWords s = (unwords . map reverse . words ) s


f . g = \x -> f (g x)