import Data.List
import Data.Char

smallesPrimeFactors n = head [x | x <- [2..n], mod n x == 0]


primeFactors n
    |  smallesPrimeFactors n == n    = smallesPrimeFactors n
    | otherwise                      = primeFactors (div n (smallesPrimeFactors n))