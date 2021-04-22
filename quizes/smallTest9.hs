import Data.Char
import Data.List


shuffle :: (b, d, a, c) -> (a, b, c, d)
-- shuffle (1,'a',"hello",True) == ("hello",1,True,'a')
-- shuffle ("haskell",False,3.14,(4,0)) == (3.14,"haskell",(4,0),False)
shuffle (x, ch, str, bol) = (str, x, bol, ch)


{-runs [] 9 == []
runs [1,4,9,16,25,36,49,64,81,100] 3 == [[1,4,9],[16,25,36],[49,64,81],[100]]
runs ["apple", "plum", "peach"] 1  == [["apple"], ["plum"], ["peach"]]
runs ["apple", "plum", "peach", "currant", "gooseberry"] 2 == [["apple", "plum"], ["peach", "currant"], ["gooseberry"]]
runs [0..23] 5 == [[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14],[15,16,17,18,19],[20,21,22,23]]
take 4 (runs [1..] 3) == [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]-}
runs :: [a] -> Int -> [[a]]
runs [] n = []
runs lst@(x:xs) n
    | n > 0  = [take n lst] ++ runs xs n
    | otherwise = runs xs n
