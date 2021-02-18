module Grow where

    -- [1, 2, 3, 4] => 1 * 2 * 3 * 4 = 24
    grow [] = 1
    grow (x:y:xs)  = (x*y) * grow xs