module Format where
    -- format 10 "haskell" == "  haskell"
    -- format 3 "haskell"  == "haskell"

    format _ [] = ""
    format n lst@(x:xs)
        | n > length(lst)  = replicate (n-length (lst)) ' ' ++ lst
        | otherwise        = (x:xs)

































       -- format 10 "haskell" == "  haskell"
    -- format 3 "haskell"  == "haskell"
    format2 :: Int -> String -> String
    format2 num lst@(x:xs) 
        | length(lst) > num && num < length(lst) =  lst
        | otherwise         = replicate (num-length(lst)) ' ' ++ lst