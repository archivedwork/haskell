-- (12, 30) `isEarlier` (13, 30)
-- (12, 30) `isEarlier` (12, 31)
-- (11, 40) `isEarlier` (12, 30)
-- not ((12, 30) `isEarlier` (12, 30))
-- not ((12, 40) `isEarlier` (12, 30))
-- not ((13, 30) `isEarlier` (12, 45))
-- not ((22,  0) `isEarlier` ( 8,  0))

isEarlier :: (Int, Int) -> (Int, Int) -> Bool
isEarlier (h1,m1)  (h2,m2)
    | h1 < h2 && m1 == m2    = True 
    | h1 == h2 && m1 < m2    = True 
    | h1 < h2 && m1 > m2     = True 
    | otherwise              = False


