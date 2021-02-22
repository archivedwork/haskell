module Insert where
   --  insert 2 [4, 5, 9]  == [2, 4, 5, 9]
   -- insert 6 [4, 5, 9]  == [4, 5, 6, 9]
   -- insert 11 [4, 5, 9] == [4, 5, 9, 11]
   -- insert 5 [4, 5, 9]  == [4, 5, 5, 9]
   -- insert 2 []         == [2]


   insert n [] = [n] 
   insert n (x:xs)
    | n <= x  = n:x: xs
    | n > x   = x  : insert n xs