module Vowel2Index where

  --  write a function  that takes in a string and replaces all the vowels [a,e,i,o,u] with their respective positions within that string.
  -- E.g: 
    -- vowel2Index "this is my string"  == "th3s 6s my str15ng"
    -- vowel2Index "Codewars is the best site in the world"  == "C2d4w6rs 10s th15 b18st s23t25 27n th32 w35rld"
    vowel2Index []     = []
    vowel2Index (x:xs) = ""


    index xs = map (\i -> i) [1..length(xs)]