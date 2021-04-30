module DropWord where 
-- dropWord ""           == ""
-- dropWord " tree"      == " tree"
-- dropWord "apple tree" == " tree"
-- dropWord "appletree"  == ""
-- takeWord "Haskell is functional " == " is functional "


dropWord :: [Char] -> [Char]
dropWord "" = ""
dropWord (x:xs)
    | x == ' '    = (x:xs)
    | otherwise   = x : dropWord xs
