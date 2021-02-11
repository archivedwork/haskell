module Inc where
    
-- Define a function which increases its integer parameter by one.
-- inc 5    == 6
-- inc 0    == 1
-- inc (-5) == (-4)

inc :: Num a => a -> a
inc x = (x + 1);



