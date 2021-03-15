import Data.Char
import Data.List


sumCubes :: Integer -> Integer 
sumCubes n = sum [x^3 | x <- [1..n]]

selfPower = [x^x | x <- [1..1000]]