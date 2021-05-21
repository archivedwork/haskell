import Data.List
import Data.Char


-- surrounded [1, 3, 5, 5]       == [3]
-- surrounded [2, 4, 8, 10]      == [4, 8]
-- surrounded [4, 3, 5, 7, 4, 9] == [5]
-- surrounded [1]    == []
-- surrounded [1, 5] == []
-- surrounded []     == []

surrounded :: [Int] -> [Int]
surrounded [] = []
surrounded [x] = []
surrounded [x,y] = []
surrounded lst@(x:y:z:xs)
    | y > x && y < z    = [y] ++ surrounded (y:z:xs)
    | otherwise = surrounded (y:z:xs)






-- marksCorrect ""
-- marksCorrect "x"
-- marksCorrect "Hello! You look great today"
-- marksCorrect "Howdy? I'm fine. Thanks"
-- not (marksCorrect "Oh! This looks bad.Dunno why")
-- not (marksCorrect "!")
-- not (marksCorrect "hello!")
-- marksCorrect "I'm sure this goes smoothly"

marksCorrect :: String -> Bool
marksCorrect "!" = False
marksCorrect "?" = False
marksCorrect "." = False
marksCorrect "" = True
marksCorrect [x] = True
marksCorrect lst@(x:y:xs)
    | x `elem` "!?." && y == ' '  = True
    | otherwise = marksCorrect (y:xs)



-- removeLast even [0, 1, 2, 3] == [0, 1, 3] 
-- removeLast odd  [0, 1, 2, 3] == [0, 1, 2] 
-- removeLast even [1, 3, 5]    == [1, 3, 5]
-- removeLast (> 0) []          == []


removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p lst = reverse (helper p (reverse lst))


helper p [] = []
helper p lst@(x:xs)
    | p x = xs
    | otherwise = x : helper p xs





-- whiteTable [] == Nothing
-- whiteTable [Chair 150 3, Chair 100 4] == Nothing
-- whiteTable [Chair 100 4, Chair 200 2, Chair 150 4] == Nothing
-- whiteTable [Table 500 "brown" "pine", Table 550 "black" "cypress", Chair 100 2] == Nothing
-- whiteTable [Table 400 "grey" "pine", Table 600 "black" "red cedar", Chair 100 2, Table 400 "white smoke" "spruce" ] == Nothing
-- whiteTable [Table 400 "grey" "pine",  Table 400 "white smoke" "spruce", Table 600 "black" "red cedar", Table 600 "white" "pine", Chair 100 2] == Just (Table 600 "white" "pine")
-- whiteTable [Chair 150 3, Table 400 "grey" "pine", Table 400 "white" "spruce", Table 400 "white smoke" "spruce", Table 600 "white" "red cedar", Table 600 "black" "red cedar", Chair 100 2] == Just (Table 400 "white" "spruce")

type Price = Int
type NoOfLegs = Int
type Color = String
type Material = String


data Furniture = Chair Price NoOfLegs | Table Price Color Material  deriving (Show,Eq)

whiteTable :: [Furniture] -> Maybe Furniture
whiteTable []  = Nothing
whiteTable ((Chair pr legs):xs) = whiteTable xs
whiteTable ((Table pr clr mat):xs)
    | clr == "white" = Just (Table pr clr mat)
    | otherwise = whiteTable xs

