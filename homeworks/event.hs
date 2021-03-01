module Event where
    -- createEvent (20, 0) 90 "soccer with friends"           == ((20, 0), (21, 30), "soccer with friends")
    -- createEvent (16, 15) 120 "rendezvous in Cafe Frei"      == ((16, 15), (18, 15), "rendezvous in Cafe Frei")
    -- createEvent (8, 30) 90 "Functional Languages practice"  == ((8, 30), (10, 0), "Functional Languages practice")
    -- createEvent (23, 30) 240 "Hiking at night"              == ((23, 30), (3, 30), "Hiking at night")

    createEvent :: (Int, Int) -> Int -> String -> ((Int, Int), (Int, Int), String)
    createEvent (h, m) sft dec
        | sft == 90     =((h,m), (h+1, sft `div` 3), dec)
        | sft == 120    = ((h,m), (h+2, m), dec)
        | sft == 240    = ((h,m), (h+6, m), dec)