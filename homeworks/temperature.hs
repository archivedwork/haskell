
type Degree = Int


data Temperature = Daytime Degree | Night Degree  deriving(Show)


isDaytime :: Temperature -> Bool
isDaytime (Daytime _ ) = True
isDaytime (Night _) = False 



type HighestDayTime = Int
type LowestNight    = Int


extremes :: [Temperature] -> (Maybe HighestDayTime, Maybe LowestNight)
extremes [Night (-5), Night (-6), Daytime 0, Daytime 3, Daytime 5, Daytime 1, Night (-7)] = (Just 5, Just (-7))
extremes [Night 5, Night 0, Daytime 1, Daytime 10, Daytime 8, Daytime 5, Night 2]         = (Just 10, Just 0)
extremes [Night 3, Night 0, Daytime 1, Daytime 10, Daytime 8, Daytime 15, Night 7]        = (Just 15, Just 0)
extremes [Night 3, Night 0, Night 7, Night 8]          = (Nothing, Just 0)
extremes [Daytime 2, Daytime 6, Daytime 10, Daytime 8] = (Just 10, Nothing)
extremes [] = (Nothing, Nothing)
