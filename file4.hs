import Data.List
import Data.Char



wordNumWithCapital :: String -> Int
wordNumWithCapital [] = 0
wordNumWithCapital lst@(x:xs) = length $ filter (\x -> isUpper (head x)) $ words lst




minList _ [] = []
minList [] _ = []
minList lst1@(x:xs) lst2@(y:ys)
    | x < y  || x == y    =  x : minList xs ys
    | otherwise  = y : minList xs ys



conditionalSum :: Num a => (a -> Bool) -> [a] -> a
conditionalSum p [] = 0
conditionalSum p lst@(x:xs)
    | p x        = x + conditionalSum p xs
    | otherwise  = conditionalSum p xs


mapMaybe :: (a -> a) -> Maybe a -> Maybe a
mapMaybe op (Just x ) = Just (op x )
mapMaybe op Nothing   = Nothing




modify :: (a -> Maybe a) -> [a] -> [a]
modify p [] = []
modify p (x:xs) = 
    case p x of
        Nothing   -> xs
        (Just y)  -> (y:xs)




type Rad = Double
type Height = Double
type Width = Double
type Length = Double


data Shape = Circle Rad | Rectangle Height Width | Square Length deriving (Show, Eq)

scale :: Double -> Shape -> Shape
scale n (Rectangle h w)  = (Rectangle (n*h) (n*w)) 
scale n (Square l)  = (Square (n*l)) 

area :: Shape -> Double
area (Circle rad) = pi * rad



data Temperature = Daytime Int | Night Int


isDayTime :: Temperature -> Bool
isDayTime (Daytime x) = True
isDayTime (Night x )  = False




extremes :: [Temperature] -> (Maybe Int, Maybe Int)
extremes [] = (Nothing, Nothing)
extremes ((Night x):(Night y): xs) 
    | x > y   = (Nothing, Just (min x y) )
    | otherwise = extremes xs

extremes ((Daytime x): (Daytime y): xs) 
    | x > y = (Just (max x y), Nothing)
    | otherwise = extremes xs