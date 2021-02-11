module Main where
-- E.g. If we have an array [1,2,3,4,6,7,8] then 1 then 2 then 3 then 4 are all consecutive but 6 is not, so that's the first non-consecutive number.



    --firstNonConsecutive :: (Eq a,Enum a) => [a] -> Maybe a
    firstNonConsecutive (n:n1:ns) 
     | (n1 - n) == 1   = firstNonConsecutive (n1:ns)
     | otherwise       = Just n1