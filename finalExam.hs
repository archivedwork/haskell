import Data.List
import Data.Char

-- ## 5. Word Insertion

-- Define a function which inserts a word into a sentence. The new word should be the `n`th word in the sentence, where `n` is an input of the function. Indexing of words starts with 0.

-- If index `n` is greater than the number of words in the sentence then insert the word at the end. If `n` is negative, insert at the beginning.

-- ```haskell
-- insertWord :: String -> Int -> String -> String
-- ```

-- ```haskell
-- insertWord "pizza" 2 "A Margherita is delicious!" == "A Margherita pizza is delicious!"
-- insertWord "Sam!" 1 "Hi" == "Hi Sam!"
-- insertWord "Sam!" 4 "Hi" == "Hi Sam!"
-- insertWord "Sam!" 4 "Hi!" == "Hi! Sam!"
-- insertWord "apple" 2 "" == "apple"
-- insertWord "apple," (-2) "pear, apricot" == "apple, pear, apricot"
-- ```

insertWord :: String -> Int -> String -> String
insertWord str _ [] = []
-- insertWord str 1 lst = lst ++  " " ++ str
-- insertWord str n lst@(x:xs)
--     | n  > length lst    = str
--     | n < 0              = str ++ lst
--     | otherwise          = insertWord str n xs


-- divideWords lst n = snd $ head $ filter (\x -> fst x == n) $ zip [0..] $ words lst









-- ## 8. Temperature

-- Define a new data structure `Temperature` which could represent temperature in two different units, `Celsius` and `Fahrenheit`. Both data constructors should take temperature of `Double` as input. Derive `Eq` and `Show` type classes.

-- Define conversion between the two units. The relationship is as follows: `1.8*celsius + 32 = fahrenheit` 

-- ```
-- convertTemperature :: Temperature -> Temperature
-- ```

-- ```
-- convertTemperature (Celsius 0) == Fahrenheit 32
-- convertTemperature (Fahrenheit 50) == Celsius 10
-- convertTemperature (Celsius (-40)) == Fahrenheit (-40)
-- ```

type Unit = Double

data Temperature = Celsius Unit | Fahrenheit Unit deriving (Show, Eq)


convertTemperature :: Temperature -> Temperature
convertTemperature (Celsius x)      = Fahrenheit (1.8*x + 32)
convertTemperature (Fahrenheit x)   =  Celsius ((x-32) / 1.8)
-- convertTemperature (Anthing  x) = Anth

-- convertTemperature (Celsius x)
--     | Celsius x == Celsius 23 = Fahrenheit (1.8*x + 32)
