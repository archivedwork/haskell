-- Car "abc-123" 5
-- Car "cde-456" 3
-- Bicycle


type PlatNumber = String 
type PeopleNumber = Int
data Vehicle = Car PlatNumber PeopleNumber | Bicycle deriving(Show)




numberOfPeople :: Vehicle -> Int
numberOfPeople Bicycle = 1
numberOfPeople (Car _ x)  = x