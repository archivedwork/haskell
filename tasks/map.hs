module Map where 


    mapUsage = map (\x -> x) [1..10]

    myMap _ [] = []
    myMap f (x:xs) = f(x) : myMap f xs



