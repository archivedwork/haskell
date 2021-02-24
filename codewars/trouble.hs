module Trouble where 

     --  trouble [1, 3, 5, 6, 7, 4, 3] 7 `shouldBe` [1, 3, 5, 6, 7, 4]
     --  trouble [4, 1, 1, 1, 4] 2       `shouldBe` [4, 1, 4]
     --  trouble [2, 2, 2, 2, 2, 2] 4    `shouldBe` [2] 

    trouble [] _           = []
    trouble [x] _          = [x]           
    trouble (x:y:xs) n
        | x+y == n        =  trouble (x:xs) n         -- here remove second element and loop
        | otherwise       =  x : trouble (y:xs) n     -- keep the second element