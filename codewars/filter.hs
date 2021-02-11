module Filter where


fltr :: (a -> Bool) -> [a] -> [a]
fltr _ []   = []
fltr fn (l:ls)
    | fn(l) == True    = l : fltr fn ls
    | otherwise        = fltr fn ls




products = ["myProd1", "meProd2"];
-- fltr fn (l:ls) = fn(l) : fltr fn ls
searchFor = filter (=="myProd1") products