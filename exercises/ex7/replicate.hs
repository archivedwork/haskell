module Replicate where
    -- replicate'5'a'== "aaaaa"
    -- replicate'10'p'== "pppppppppp"

    replicate1 0 _ = ""
    replicate1 n chr = chr : replicate1 (n-1) chr