import Data.Char

-- isIdentifierStart 'a'
-- isIdentifierStart 'x'
-- isIdentifierStart '_'
-- not (isIdentifierStart 'A')
-- not (isIdentifierStart 'P')
-- not (isIdentifierStart '#')
-- not (isIdentifierStart '5')


isIdentifierStart :: Char -> Bool
isIdentifierStart c | c `elem` ['a'..'z'] = True 
isIdentifierStart c | c == '_' = True 
isIdentifierStart _ = False


-- isIdentifierPart 'e'
-- isIdentifierPart 'v'
-- isIdentifierPart 'E'
-- isIdentifierPart 'N'
-- isIdentifierPart '2'
-- isIdentifierPart '_'
-- not (isIdentifierPart ' ')
-- not (isIdentifierPart '#')
-- not (isIdentifierPart ';')

isIdentifierPart :: Char -> Bool
isIdentifierPart c 
    | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` "_" || c `elem` ['0'..'9'] = True 
    | otherwise                                                                     = False




-- isReserved "module"
-- isReserved "import"
-- not (isReserved "")
-- not (isReserved "even")

isReserved :: [Char] -> Bool
isReserved st 
    | st == "if" || st == "then" || st == "else" || st == "module" || st == "import" = True 
    | otherwise = False



-- isValid "even2"
-- isValid "_odd_"
-- isValid "toUpper"
-- isValid "elem"
-- isValid "f"
-- not (isValid "")
-- not (isValid "import")
-- not (isValid "True")
-- not (isValid "2elem")
-- not (isValid "mod$")

isValid :: [Char] -> Bool
isValid st@(x:xs) 
    | isLower x || x `elem` "_" = True 
    | otherwise = not (isReserved st)