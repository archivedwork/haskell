module EverySecond where 

    -- everySecond "Haskell" == "akl"
    -- everySecond "H"       == ""
    -- everySecond "java"    == "aa"
    -- everySecond ""        == ""
    everySecond ""                          = ""
    everySecond (x:xs) | length (x:xs) == 1 = ""
    everySecond (x:y:xs)                    = y : everySecond xs