module Fib where 

-- Determine thenth Fibonacci number
     -- fib 0 == 0
     -- fib 1 == 1
     -- fib 2 == 1
     -- fib 4 == 3
     -- fib 5 == 5

     fib 0 = 0
     fib 1 = 1
     fib 2 = 1
     fib x = fib(x-2) + fib(x-1)