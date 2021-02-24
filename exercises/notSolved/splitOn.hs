module SplitOn where

    -- splitOn''"haskell is cool"      == ["haskell", "is", "cool"]
    -- splitOn'/'"haskell/gyak/gyak.hs" == ["haskell", "gyak", "gyak.hs"]
    -- splitOn'/'"haskell"              == ["haskell"]
    -- splitOn'/'""                     == []

    splitOn c [] = []
    splitOn c (x:xs)
        | c == x    =  takeWord c (x:xs)
        | otherwise = splitOn c xs



    takeWord c [] = []
    takeWord c (x:xs)
        | c == x   = (x:xs)
        | otherwise  =x :  takeWord c xs