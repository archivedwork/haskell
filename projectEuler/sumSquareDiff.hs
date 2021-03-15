
sumSquares = sum [x^2 | x <- [1..100]]
-- or 
sumSquares2 = sum (map (^2) [1..100])

squareSum = (sum [x | x <- [1..100]])^2

difference = squareSum - sumSquares