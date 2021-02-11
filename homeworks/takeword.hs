module TakeWord where

-- takeWord ""           == ""
-- takeWord " tree"      == ""
-- takeWord "apple tree" == "apple"
-- takeWord "appletree"  == "appletree"
-- takeWord "Haskell is functional " == "Haskell"

takeWord :: [Char] -> [Char]
takeWord "" = ""
takeWord (x:xs)
    | x == ' '   = ""
    | otherwise  = x : takeWord xs




