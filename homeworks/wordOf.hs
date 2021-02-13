module WordOf where

-- takeWord ""           == ""
-- takeWord " tree"      == ""
-- takeWord "apple tree" == "apple"
-- takeWord "appletree"  == "appletree"
-- takeWord "Haskell is functional " == "Haskell"
    takeWord "" = ""
    takeWord (x:xs)
        | x == ' '     = ""
        | otherwise    = x : takeWord xs


-- dropWord ""           == ""
-- dropWord " tree"      == " tree"
-- dropWord "apple tree" == " tree"
-- dropWord "appletree"  == ""
-- dropWord "Haskell is functional " == " is functional "

    dropWord :: [Char] -> [Char]
    dropWord "" = ""
    dropWord (x:xs)
        | x == ' '    = helperDrop (x:xs)
        | otherwise  = dropWord xs

    helperDrop (x:xs) = drop (length(takeWord (x:xs))) (x:xs)


-- wordsOf ""                                             == []
-- wordsOf "hello"                                        == ["hello"]
-- wordsOf "hello  "                                      == ["hello"]
-- wordsOf "apple tree"                                   == ["apple","tree"]
-- wordsOf "   Recursive    functions   are  common in   Haskell.    "            == ["Recursive","functions","are","common","in","Haskell."]

    wordsOf "" = []
    wordsOf (x:xs)
        | x == ' '     = [dropWord (x:xs)]
        | x /= ' '     = [takeWord (x:xs)]
       
       
       
       --  | otherwise    = [takeWord (x:xs)]