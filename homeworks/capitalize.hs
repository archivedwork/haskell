module Capitalize where
    import Data.Char
    import Data.List



    upperFirst = concatMap (\(c:cs) -> toUpper c : cs) . groupBy (\a b -> isSpace a == isSpace b)

