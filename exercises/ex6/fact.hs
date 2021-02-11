module Fact where 

-- Define the factorial function recursively.
-- fact 0 == 1
-- fact 1 == 1
-- fact 3 == 6
-- fact 6 == 720

fact 0 = 1
fact 1 = 1
fact x = x * fact (x - 1)