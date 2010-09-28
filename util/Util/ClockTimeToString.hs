-- | This module implements displaying ClockTime as a String which does NOT
-- depend on the time-zone.
module Util.ClockTimeToString(
   clockTimeToString, -- :: ClockTime -> String
   stringToClockTime, -- :: String -> ClockTime
   ) where

import System.Time

import Util.ExtendedPrelude

-- | Convert a ClockTime to a String.
-- This has the format
--    \<optional sign\>\<digits\>+\<digits\>
-- where the digits encode two integers N1 and N2 (in order) representing
-- the time elapsed since 00:00:00 UTC on 1 Jan 1970.  This will be
-- N1 + (N2 \/ 10^12) seconds.  0<=N2<10^12.
clockTimeToString :: ClockTime -> String
clockTimeToString (TOD n1 n2) = show n1 ++ "+" ++ show n2

-- | Convert a validly formatted String to a ClockTime.
stringToClockTime :: String -> ClockTime
stringToClockTime s = case splitByChar '+' s of
   [n1s,n2s] -> TOD (read n1s) (read n2s)
   _ -> error "Badly formatted clock time"
