import Data.List
import Data.Char


zipF :: [a->b] -> [a] -> [b]
zipF [] _ = []
zipF _ [] = []
zipF (f:fx) (y:ys) = f y : zipF fx ys


test_zipF = [
    zipF [even] [4,5,6]                  == [True],
    zipF [(+1),(*2),(`div` 3)] []        == [],
    zipF [] [4,5,6]                      == [],
    zipF [(+1),(*2),(`div` 3)] [4,5,6]   == [(4+1),(5*2),(6 `div` 3)],
    zipF [(+1),(*2),(`div` 3)] [4,5,6,7] == [5,10,2],
    zipF [(+1),(*2)] [4,5,6]             == [5,10],
    zipF [(+1),(*2),(`div` 3)] [1..]     == [2,4,1],
    zipF [(*i) | i <- [2..]] [4,5,6]     == [8,15,24],
    take 10 (zipF [(+i) | i <- [10..]] [1..]) == [11,13,15,17,19,21,23,25,27,29]]





propToMaybe :: (a -> Bool) -> a -> Maybe a
propToMaybe p x 
    | p x = Just x
    | otherwise = Nothing


test_prop = [
    propToMaybe even 2     == Just 2,
    propToMaybe even 1     == Nothing,
    propToMaybe null []    == Just [],
    propToMaybe null [1..] == Nothing]