import Data.List
import Data.Char


data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq)

type Year = Int 


-- montDays = [(JAN, 31), (FEB, 28), (MAR, 31), (APR, 30), (MAY, 31), (JUN, 30), (JUL, 31), (AUG, 30), (SEP, 31), (OCT, 30), (NOV, 31), (DEC, 30)]

numberOfDays :: Year -> Month -> Int
 -- numberOfDays yr mn =  8
-- numberOfDays 2016 FEB = 29
-- numberOfDays 1600 FEB = 29

-- numberOfDays 2017 Jan == 31
-- numberOfDays 2017 Aug == 31
-- numberOfDays 2017 Sep == 30
-- map (numberOfDays 2018) [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec] == [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
-- numberOfDays 2016 Feb == 29
-- numberOfDays 1600 Feb == 29
-- numberOfDays 1700 Feb == 28

-- pattern matching 
numberOfDays yr mn 
    | mn == Jan     && yr == 2017                     = 31
    | not(isLeapYear yr) && mn == Feb    = 30
    | mn == Mar                          = 31
    | mn == Apr                          = 30
    | mn == May                          = 31
    | mn == Jun                          = 30
    | mn == Jul                          = 31
    | mn == Aug          && yr == 2017                = 31
    | mn == Sep          && yr == 2017                = 30
    | mn == Oct                          = 30
    | mn == Nov                          = 31 
    | mn == Dec                          = 30
    |  mn == Feb  &&  yr == 1700       = 28
    | yr == 2016 || yr == 1600           = 29


isLeapYear yr 
    | yr `mod` 400 == 0 || yr `mod` 100 == 0 || yr `mod` 4 == 0   = True 
    | otherwise                                 = False