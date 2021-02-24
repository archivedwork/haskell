module Switcheroo where

    switcheroo [] = []
    switcheroo (x:xs)
        | x == 'a'    = 'b' : switcheroo xs
        | x == 'b'    = 'a': switcheroo xs
        | otherwise  = x: switcheroo xs



    swithH = map (\x -> 
        if(x == 'a')
            then 'b'
        else 
            if(x=='b')
                then 'a' 
            else 
                x      
        )


    switchHe str = map check str

    check st 
        | st == 'a' = 'b'
        | st == 'b' = 'a'
        | otherwise = st