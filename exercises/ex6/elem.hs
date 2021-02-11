module Elem where
    elem1 c [] = False
    elem1 c (x:xs)
        | c == x    = True 
        | otherwise = elem1 c xs