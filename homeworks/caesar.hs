import Data.List
import Data.Char


table = zip ['a'..'w'] [(toEnum(97+3) :: Char)..] ++ [(' ', ' ')] ++ [('x', 'a')]  ++ [('y', 'b')]  ++ [('z', 'c')]

-- shift table ' ' == ' '
-- shift table 'a' == 'd'
-- shift table 'b' == 'e'
-- shift table 'x' == 'a'
-- shift table 'y' == 'b'
-- shift [('b', 'g'), ('c', 'h'), ('a', 'f')] 'a' == 'f'
-- shift [('b', 'g'), ('c', 'h'), ('a', 'f')] 'b' == 'g'
-- shift [('b', ' '), ('c', '!'), ('a', '#')] 'a' == '#'
-- shift [('b', ' '), ('c', '!'), ('a', '#')] 'd' == '?'
-- shift [] 'a' == '?'
shift [] c = '?'
shift ((a,b):xs) c
    |c == a   = b
    | otherwise = shift xs c


-- encrypt table "abcd"           == "defg"
-- encrypt table "hello"          == "khoor"
-- encrypt table "haskell is fun" == "kdvnhoo lv ixq" 
-- encrypt table "wxyz"           == "zabc"
-- encrypt table ""               == ""
-- encrypt [('p', 'p'), ('g', 'n'), ('u', 'a')] "pug" == "pan"
-- encrypt [] "pug" == "???"

encrypt _ [] = ""
encrypt aa@((a,b):xs) (y:ys) = shift aa y : encrypt aa ys
encrypt [] (y:ys) = '?' : encrypt [] ys --  error
