import Data.List
import Data.Char



reverseInt = reverseHelper 0

reverseHelper acc 0 = acc
reverseHelper acc n = reverseHelper (acc * 10 + (mod n 10)) (div n 10)



isPalindrome n = n == (reverseInt n)