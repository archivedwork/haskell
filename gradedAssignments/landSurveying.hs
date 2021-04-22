import Data.Char
import Data.List

data Measure = Land Int | Sea  deriving (Show, Eq)


-- measurements above land denoted by positive Int
-- measurements above sea is denoted by         0

type Measures = [Measure]


testMeasures1 :: Measures
testMeasures1 = []

testMeasures2 :: Measures
testMeasures2 =
  [ Land 300, Land 200, Land 400, Sea, Sea, Land 300, Land 200, Land 150, Sea
  , Land 40, Land 80, Land 500, Land 650, Land 890, Land 300, Sea, Sea, Sea
  , Sea, Land 40 ]

testMeasures3 :: Measures
testMeasures3 =
  [ Sea, Sea, Sea, Land 50, Land 100, Land 10, Sea, Land 30, Land 80, Land 350
  , Land 700, Land 980, Land 600, Land 200, Land 40, Sea, Sea, Sea, Sea ]



-- Measurement above land
isLand :: Measure -> Bool
isLand Sea = False
isLand (Land _) = True


-- Measurement above sea
hasSea :: Measures -> Bool
hasSea [] = False
hasSea lst = Sea `elem` lst


-- Detecting islands
splitSegments :: Measures -> [Measures]
splitSegments mes = groupBy (==) mes
