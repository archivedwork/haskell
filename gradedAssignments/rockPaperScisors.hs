import Data.Char
import Data.List


-- 0 -> rock
-- 1 -> paper
-- 2 -> scissors


type Sign = Int
type Play = [Sign]
type Rounds = (Play, Play)

validSigns :: (Sign -> Sign -> a) -> Sign -> Sign -> a
validSigns f x y
    | x `elem` [0,1,2] && y `elem` [0,1,2] = f x y
    | otherwise                        = error("wrong sign")




-- Rock beats scissors.
-- 0 beats 2                 == true

-- Paper beats rock.
-- 1 beats 0                == true

-- Scissors beat paper.
-- 2 beats 1               == true



beats :: Sign -> Sign -> Bool
beats x y
    | (validSigns (\x y -> x) x y) == 0 && (validSigns (\x y -> y) x y) == 2 = True
    | (validSigns (\x y -> x) x y) == 1 && (validSigns (\x y -> y) x y) == 0 = True
    | (validSigns (\x y -> x) x y) == 2 && (validSigns (\x y -> y) x y) == 1 = True
    | otherwise                                                              = False




isDraw :: Sign -> Sign -> Bool
isDraw x y
    | (validSigns (\x y -> x) x y) == 0 && (validSigns (\x y -> y) x y) == 0 = True
    | (validSigns (\x y -> x) x y) == 1 && (validSigns (\x y -> y) x y) == 1 = True
    | (validSigns (\x y -> x) x y) == 2 && (validSigns (\x y -> y) x y) == 2 = True
    | otherwise                                                              = False



-- 1     1st player beats    2nd
-- 0     1st player draw     2nd
-- -1    1st player defeated 2nd

result :: Sign -> Sign -> Int
result x y
    |  x == y            =  0
    | (x == 0 && y == 1) || (x == 1 && y == 2) || (x == 2 && y == 0) = -1
    | otherwise         = 1



-- 0 : the game is a draw (both players won the same number of rounds)
-- 1 : the first player won the game (the first player won more rounds than the second one)
-- 2 : the second player won the game

tournament :: Rounds -> Int
tournament (player1_List, player2_List)
    | length(player1_List) == length(player2_List) = tournamentHelp (player1_List, player2_List)
    | otherwise   = error("lists length did not match")


tournamentHelp ([], []) = 0
tournamentHelp (lst1, lst2)
    | length (filter (==True) (playerOneRounds lst1 lst2)) == length ( filter (==True) (playerTwoRounds lst1 lst2)) = 0
    | length (filter (==True) (playerOneRounds lst1 lst2)) > length ( filter (==True) (playerTwoRounds lst1 lst2))  = 1
    | otherwise                                                                = 2

playerOneRounds :: Play -> Play -> [Bool]
playerOneRounds [] [] = []
playerOneRounds (x:xs) (y:ys) = (x `beats` y) : playerOneRounds xs ys


playerTwoRounds :: Play -> Play -> [Bool]
playerTwoRounds (x:xs) (y:ys) = (y `beats` x) : playerTwoRounds xs ys
playerTwoRounds [] [] = []

{-
-- tournament_test (

[1 0, 2 2, 0 1, 1 0, 2 0, 0 1, 1 2 , 1 1, 0 1, 1 2]
[T  , F,     F,  T , F  , F  ,  F  , F  , F  , F ]   = 1st player won in 2 rounds

[0 1, 2 2, 1 0, 0 1, 0 2, 1 0, 2 1, 1 1, 1 0, 2 1]
[F  , F   , T  , F , T  , T  , T  , F  , T   , T]    = 2nd player won in 6 rounds

-}

