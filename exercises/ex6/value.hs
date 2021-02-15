module Value where
    -- value 5 [(0,"c++"),(5,"python"),(4,"rust")] == "python"
    -- value 4 [(0,"c++"),(5,"python"),(4,"rust")] == "rust"
    -- value 4 [(0,"c++"),(5,"python"),(4,"go")]   == "go"

    -- value n []        = ""
    value n as@[(x, xs)]
        | n == x        = xs
        | otherwise     = value n (tail as)

    