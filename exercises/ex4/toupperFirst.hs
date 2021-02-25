module ToupperFirst where


    import Data.Char
    -- toUpperFirst ""               == ""
    -- toUpperFirst "finn the human" == "Finn the human"
    -- toUpperFirst "jake"           == "Jake"

    toUpperFirst ""    = ""
    toUpperFirst xs    = [toUpper (x) | x <- xs, x == head (xs)] ++ tail (xs)
