{- #########################################################################

MODULE        : Date
AUTHOR        : Einar W. Karlsen
                University of Bremen
                email:ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Date Type and a couple of useful operations on dates.


   ######################################################################### -}


module Date (
        Date(..),
        getToDay
        ) 
where

import Char
import Time(toCalendarTime, getClockTime, ClockTime, CalendarTime(..))
import Debug(debug)


-- --------------------------------------------------------------------------
-- Date Type
-- --------------------------------------------------------------------------
        
data Date = Date Int Int Int deriving (Ord, Eq)


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

getToDay :: IO Date 
getToDay =
   do
        t <- getClockTime
        c <- toCalendarTime t
        return (Date (ctYear c) (nMonth (ctMonth c)) (ctDay c))
   where
      nMonth month = fromEnum month + 1

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Show Date where
   showsPrec d (Date year mon day) r = 
        (fillZero 4 (show year)) ++ "." ++ 
        (fillZero 2 (show mon)) ++ "." ++ 
        (fillZero 2 (show day)) ++ r

instance Read Date where
   readsPrec p b =
     case dropWhile (isSpace) b of
         [] -> [(Date 0 0 0,[])]
         xs -> let [y,m,d] = map read (split '.' xs) in 
                        [(Date y m d,[])]

{- this function should really be taken from Util -}

split :: Char -> String -> [String]
split ch s = case dropWhile (\x -> x == ch) s of
                "" -> []
                s' -> w : split ch s''
                      where (w,s'') = break (\x -> x == ch) s' 


fillZero n str = fillChar '0' n str

fillChar ch n str = 
        let l = length str in
                if l <= n then
                        (take (n - l) (repeat ch)) ++ str
                else
                        take n str

