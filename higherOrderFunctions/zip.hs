module Zip where
    -- zip :: [a] -> [b] -> [(a, b)]

    zipp [] []                   = []
    zipp (x:xs) (y:ys)           =  [(x,y)] ++ zipp xs ys



    -- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zippWith op [] _ = []
    zippWith op _ [] = []
    zippWith op (x:xs) (y:ys) = [op x y] ++ zippWith op xs ys



    