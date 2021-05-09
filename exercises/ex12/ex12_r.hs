import Data.List
import Data.Char


-- keep1 :: (Int -> Bool) -> [a] -> [a]
-- keep1 f lst = [v | (i,v) <- zip[0..] lst, f i == True]

p_h p (i,v) = p i

-- (i,v)
hl lst = zip[0..] lst
-- keep1 p  lst = map snd $ filter (p (fst $ hl lst)) lst 
