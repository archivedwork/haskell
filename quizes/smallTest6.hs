-- smallTest6

-- punctuation '?'
-- punctuation '!'
-- punctuation '.'
-- not (punctuation 'x')
-- not (punctuation ' ')
-- not (punctuation 'K')

import Data.Char
import Data.List

punctuation :: Char -> Bool
punctuation '?' = True 
punctuation '!' = True
punctuation '.' = True 
punctuation _   = False




firstTwo :: [Bool] -> Bool
firstTwo [] = False
firstTwo [False] = False
firstTwo [True]  = True
firstTwo [False, True] = True 
firstTwo [True, False] = True
firstTwo [False, False] = False
firstTwo [True, True]   = False
firstTwo (True : y:xs) | y==False = True
firstTwo (False : y:xs) | y== True  = True
firstTwo (True: xs) = False


onAxis (0, 0) = True
onAxis (0, 100) = True
onAxis (50, 0) = True
onAxis (-12, 0) = True
onAxis (4, 5)  = False
onAxis (9, 13) = False