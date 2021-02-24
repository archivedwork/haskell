module Format where
    -- format 10 "haskell" == "  haskell"
    -- format 3 "haskell"  == "haskell"

    format _ [] = ""
    format n lst@(x:xs)
        | n > length(lst)  = replicate (n-length (lst)) ' ' ++ lst
        | otherwise        = (x:xs)



    