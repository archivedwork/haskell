import Data.Char
import Data.List


getWords = words

titleCase :: String -> String
titleCase str = unwords [ toUpper x :  map toLower aa | xs <- getWords str, x <- [head xs], aa <- [tail xs]]
