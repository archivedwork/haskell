

evenFib = sum $ filter even $ takeWhile (<= 4000000) fibSeries

fibSeries = 1 : 2 : zipWith (+) fibSeries (tail fibSeries)