import Data.Char
import Data.List

dropWhile' p (x:xs)
    | p x  = dropWhile' p xs
    | otherwise = x : dropWhile p xs


dropSpaces [] = []
dropSpaces lst = dropWhile (\x -> isSpace x) lst


trim lst = reverse $ dropSpaces $ reverse $ dropSpaces lst


-- zipWith' min [1,9,2,5] [5,0,3,8] == [1,0,2,5]
zipWith' op [] _ = []
zipWith' op _ [] = []
zipWith' op (x:xs) (y:ys) = op x y : zipWith op xs ys


test_zip = [
    zipWith' min [1,9,2,5] [5,0,3,8] == [1,0,2,5],
    zipWith' min [1,0,3] [5,2,10,1] == [1,0,3],
    zipWith' (*) [2,0,6] [1,5,4,9] == [2,0,24]]


test_dotProduct = [
    dotProduct [1, 2] [3, 4] == 11,
    dotProduct [2, 2, 2] [5, 4, 3] == 24,
    dotProduct [3] [2]             == 6,
    dotProduct [1..10] [1..10]     == 385]

dotProduct lst1 lst2 = sum $ zipWith (*) lst1 lst2


-- remove duplicates
uniq [] = []
uniq lst@(x:xs) = map (\x -> head x) $ group $ sort lst


repeated [] = []
repeated lst@(x:xs) = map (\x -> head x) $ filter (\x -> length x > 1) $ group $ sort lst