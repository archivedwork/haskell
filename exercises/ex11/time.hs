data Time = T Int Int
  deriving (Show)

showTime :: Time -> String
showTime (T h m) = show h ++ "." ++ show m




eqTime :: Time -> Time -> Bool
eqTime (T h1 m1) (T h2 m2)
  | h1 == h2 && m1 == m2    = True
  | otherwise               = False




isEarlier :: Time -> Time -> Bool
isEarlier (T h1 m1) (T h2 m2)
  | h1 <= h2 && m1 < m2   = True
  | h1 >= h2 || m2 <= m1  = False



{-
(either t1<t2<t3 or t3<t2<t1).
isBetween (T 12 30) (T 12 40) (T 12 50)
isBetween (T 12 50) (T 12 40) (T 12 30)
not (isBetween (T 12 30) (T 12 15) (T 12 50))
not (isBetween (T 12 30) (T 10 00) (T 12 50))
not (isBetween (T 12 30) (T 15 00) (T 12 50))-}

isBetween :: Time -> Time -> Time -> Bool
isBetween (T h1 m1)  (T h2 m2)  (T h3 m3) = isEarlier (T h1 m1) (T h3 m3) || isEarlier (T h3 m3) (T h1 m1)


data Timet = AM Int Int | PM Int Int
  deriving (Show)

showUSTime :: Timet -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

