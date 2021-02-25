module IsLetter where



    -- isLetter 'a'
    -- isLetter'A'
    -- isLetter'b'
    -- isLetter'X'
    -- not (isLetter'?')

     isLetter c 
        | c `elem` ['a'..'z'] = True 
        | c `elem` ['A'..'Z'] = True 
        | otherwise         = False