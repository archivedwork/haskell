


and' x y
    | x == True && y == True   = True
    | otherwise                = False




or' x y
    | x == False && y == False    = False
    | otherwise                    = True




add2 0 0 = (0,0)
add2 1 0 = (1,0)
add2 1 1 = (0,1)


-- paren'('')'
paren '(' ')' = True

paren '[' ']' = True
paren '{' '}' = True
paren '(' ']' = False
paren '(' '(' = False
paren '[' 'a' = False




calc (x, '+', y) = x+y
calc (x, '-', y) = x-y
calc (x, '*', y) = x*y
calc (x, '/', y) = x `div` y




isSpace ' ' = True
isSpace _ = False



replaceLine '\n' = ' '
replaceLine x = x


replaceNew [] = []
replaceNew (x:xs)
    | x == '\n'  = replaceLine x : replaceNew xs
    | otherwise  = x : replaceNew xs



swapThisAndThat :: String -> String
swapThisAndThat "this" = "that"
swapThisAndThat "that" = "this"
swapThisAndThat x = x
