module DeliveryCost where

    
   -- If the order contains a heavy machine (at least 50 kg), then the delivery is free for the total order.
   --  Otherwise, if the total price of machines in the order reaches 30000 Forints, the delivery cost is 5000 Forints.
   --  Every other order costs 10000 Forints to deliver.

    -- deliveryCost [("kettle", 1.1, 6999), ("waching machine", 50, 79999), ("toaster", 1.5, 4999)] == 0
    -- deliveryCost [("blender", 0.7, 15999), ("fridge", 65, 119999), ("toaster", 1.5, 4999), ("chocolate fountain", 2.4, 19999)] == 0
    -- deliveryCost [("kettle", 1.1, 6999), ("boiler", 51, 18999), ("iron", 2, 1999)] == 0
    -- deliveryCost [("blender", 0.7, 15999), ("toaster", 1.5, 4999), ("chocolate fountain", 2.4, 19999)] == 5000
    -- deliveryCost [("milk frother", 0.3, 1999), ("microwave oven", 30.0, 14999), ("dehydrator", 15.0, 12999)] == 10000
    -- deliveryCost [] == 0


    -- psuedo code
    -- if weight >= 50 
    --     then 0
    -- if totalPrice >= 30000 
    --     then 5000
    -- else 
    --     10000
    deliveryCost :: [(String, Float, Int)] -> Int
    deliveryCost [] = 0
    deliveryCost list@((name, weight, price):xs)
        | checkWeight (myList list) = 0
        | calculateCost price xs >= 30000 = 5000
        | otherwise    = 10000

    myList :: [(String, Float, Int)] -> [Float]
    myList [] = []
    myList (x@(a, y, z):xs) = y : myList xs


    calculateCost :: Num p => p -> [a] -> p
    calculateCost p []      = 0 
    calculateCost p (x:xs)  =  p + calculateCost p xs 



    -- checkWeight :: (Ord a, Num a) => [a] -> Bool
    checkWeight :: (Ord a, Num a) => [a] -> Bool
    checkWeight [] = False
    checkWeight  (x:xs)
        | x >= 50  = True
        | otherwise = checkWeight xs