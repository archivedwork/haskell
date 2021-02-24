module Last where
    -- last'[4,5,9]  == 9
    -- last'[5]      == 5
    -- last'[1..100] == 100
    
    last1 (x:xs) = head (reverse (x:xs))



    -- reverse1 [1,2,3,4] === [4,3,2,1]
    reverse1 [] = []
    reverse1 (x:xs) = "" 