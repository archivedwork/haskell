module WarnTheSheep where
    import Data.List (findIndex, reverse)


    warnTheSheep xs =
        case findIndex (=="wolf") $ reverse xs of
        Just 0 -> "Pls, go away and stop eating my sheep"
        Just n -> "Oi! sheep number " ++ show n ++ "! You are about to be eaten by a wolf."
        Nothing -> ""
