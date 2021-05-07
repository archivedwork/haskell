import Data.Char
import Data.List


type ABC = [Char]

abc :: ABC
abc = ['A'..'Z']

type Riddle       = String
type RightGuesses = [Char]
type WrongGuesses = [Char]
type State        = (Riddle, RightGuesses, WrongGuesses)


------------------------- Test Cases --------------------
test_isvalid = [
    isValidLetter 'a' abc        == True
  , isValidLetter 'X' abc        == True
  , isValidLetter ' ' abc        == False
  , isValidLetter '$' "*]-$><"   == True
  , isValidLetter 'E' ['a'..'z'] == True]


isValidLetter :: Char -> ABC -> Bool
isValidLetter chr str
    | chr == ' '     = False
    | isUpper(chr)   = toLower chr `elem` (map (\x -> toLower x) str)
    | isLower(chr)   = (toUpper(chr)) `elem` str
    | otherwise      = chr `elem` str



------------------------- Test Cases --------------------

-- State = (Riddle, RightGuesses, WrongGuesses)
test_startState =
  [ startState abc ""                == ("","","")
  , startState abc "SOS"             == ("SOS","","")
  , startState abc "Save Our Souls"  == ("SAVE OUR SOULS","","")
  ]


startState :: ABC -> String -> State
startState str1 [] = ([], [], [])
startState str1 str2@(a:as) 
    | isValidLetter a str1  = (map (\x -> toUpper x) str2, [], [])
    | not (isValidLetter a str1) = (undefined, [], [])
    | otherwise                  = startState str1 as




------------------------- Test Cases --------------------

test_guessLetter =
  [ guessLetter abc 'a' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'A' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'k' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","","K")
  , guessLetter abc 'a' (guessLetter abc 'a' (startState abc "Save Our Souls")) == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'K' (guessLetter abc 'k' (startState abc "Save Our Souls")) == ("SAVE OUR SOULS","","K")
  , guessLetter abc 'v' ("SAVE OUR SOULS", "A", [])       == ("SAVE OUR SOULS","VA","")
  , guessLetter abc 'k' ("SAVE OUR SOULS", "VA", [])      == ("SAVE OUR SOULS","VA","K")]


    -- If the guessed letter, regardless of case, is not part of the alphabet then return undefined.

    -- If the guessed letter has already occured before, meaning it is among guessed letters then do not update the state.

    -- If the guessed letter is a letter of the phrase then place it in the list of correct guesses.

    -- If the guessed letter is not a letter of the phrase then place it in the list of missed guesses.


-- r  riddle,
-- rg rightGusses,
-- wg wrongGusses

guessLetter :: ABC -> Char -> State -> State
guessLetter str1 c (r, rg, wg)
    | toUpper c `elem` rg || toUpper c `elem` wg = (r, rg, wg)
    | isValidLetter c str1  && toUpper(c) `elem` r     = (r, [toUpper(c)]++rg, wg)
    | not (isValidLetter c str1) = undefined
    | otherwise                  = (r, rg, [toUpper(c)]++wg)


all_test = [
    test_isvalid,
    test_startState,
    test_guessLetter]