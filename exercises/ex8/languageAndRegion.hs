module LanguageAndRegion where
    -- langAndRegion "en-US" == ("en", "US")
    -- langAndRegion "en-GB" == ("en", "GB")
    -- langAndRegion "fr-CA" == ("fr", "CA")

    
    langAndRegion (x:xs)= (takeWord (x:xs), dropWord (x:xs))

    takeWord "" = ""
    takeWord (x:xs)
        | x == '-'  = ""
        | otherwise = x : takeWord xs


    dropWord "" = ""
    dropWord (x:xs)
        | x == '-'    =  (xs)
        | otherwise   = dropWord xs