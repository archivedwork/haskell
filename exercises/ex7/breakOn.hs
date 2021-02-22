module BreakOn where 
    -- breakOn '' "haskell is cool"      == ("haskell", " is cool")
    -- breakOn '' "is cool"              == ("is", " cool")
    -- breakOn '/' "haskell/gyak/gyak.hs" == ("haskell", " gyak/gyak.hs")


    breakOn c (x:xs) = (takeWord c (x:xs), dropWord c (x:xs))



    takeWord c (x:xs)
        | c == x = ""
        | otherwise = x : takeWord c xs

    
    dropWord c (x:xs)
        | c == x   = drop (length (takeWord c (x:xs))) (x:xs)
        | otherwise = dropWord c xs