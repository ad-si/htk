{- This module implements user-defined filtering on VersionInfo values,
   used for displaying particular versions.

   Current filtering allows filtering by (a) date; (b) deletion count.
   -}
module VersionInfoFilter(
   VersionInfoFilter,

   defaultVersionInfoFilter, 
      -- :: VersionInfoFilter
   filterVersionInfo, 
      -- :: VersionInfoFilter -> VersionInfo -> Bool
   readVersionInfoFilter,
      -- :: IO (Maybe VersionInfoFilter)

   -- Functions for controlling the deletion count
   upDeleteCount, -- :: VersionInfo -> WithError VersionInfo
   downDeleteCount, -- :: VersionInfo -> WithError VersionInfo
   
   ) where

import Debug.Trace

import Char
import Time

import ExtendedPrelude
import Computation
import BinaryExtras (initialClockTime)

import HTk (value,height,width)
import SimpleForm

import VersionInfo

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

data VersionInfoFilter = VersionInfoFilter {
   maxAllowedDeletionCount :: Int,
   notBefore :: ClockTime
   } deriving (Show)

defaultVersionInfoFilter :: VersionInfoFilter
defaultVersionInfoFilter = VersionInfoFilter {
   maxAllowedDeletionCount = 0,
   notBefore = initialClockTime
   }

-- ----------------------------------------------------------------------
-- Applying the filter
-- ----------------------------------------------------------------------

-- | Returns 'True' if versionInfo passes filter.
filterVersionInfo :: VersionInfoFilter -> VersionInfo -> Bool
filterVersionInfo filter versionInfo =
   let
      dcount = case fromWithError (toDeletionCount versionInfo) of
         Left mess -> trace ("Error reading deletion count: " ++ mess) 0
         Right count -> count
   in
      (dcount <= maxAllowedDeletionCount filter)
      && ((timeStamp . server $ versionInfo) >= notBefore filter)

-- ----------------------------------------------------------------------
-- Reading the VersionInfoFilter
-- ----------------------------------------------------------------------

readVersionInfoFilter :: IO (Maybe VersionInfoFilter)
readVersionInfoFilter = 
   doForm "Set Version Filter" versionInfoFilterForm


versionInfoFilterForm :: Form VersionInfoFilter
versionInfoFilterForm =
   fmap
      (\ (dcount,((),notBefore)) -> VersionInfoFilter {
         maxAllowedDeletionCount = dcount,
         notBefore = notBefore
         })
      (deletionCountForm //
         (nullForm "Not before" \\ dateForm))

-- ----------------------------------------------------------------------
-- Deletion Count Form
-- ----------------------------------------------------------------------

deletionCountForm0 :: Form Int
deletionCountForm0 = newFormEntry "Maximum Allowed Deletion Count" 0

deletionCountForm :: Form Int
deletionCountForm =
   guardForm
      (>=0) 
      "Maximum Allowed Deletion Count must be at least 0"
      deletionCountForm0

-- ----------------------------------------------------------------------
-- Simple form for prompting for a date
-- ----------------------------------------------------------------------

dayOfMonthForm0 :: Form String
dayOfMonthForm0 = editableTextForm0 [value " 1",height 1,width 2]

dayOfMonthForm :: Form Int
dayOfMonthForm = mapForm
   (\ dom -> case readCheck dom of
      Just i | i >=1 && i<=31
         -> return i
      _ -> fail "Day of month must be number from 1 to 31"
      )
   dayOfMonthForm0

monthForm :: Form Month
monthForm = newFormOptionMenu2
   (map (\ month -> (show month,month)) [January .. December])

yearForm0 ::  Form String
yearForm0 = editableTextForm0 [value "2000",height 1,width 4]

yearForm :: Form Int
yearForm = mapForm
   (\ y -> case readCheck y of
      Just i | i >=2000
         -> return i
      _ -> fail "Year must be at least 2000"
      )
   yearForm0

dateForm :: Form ClockTime
dateForm =
   let
      dmy :: Form ((Int,Month),Int)
      dmy = (dayOfMonthForm \\ monthForm) \\ yearForm

      calendarForm :: Form CalendarTime
      calendarForm =
         fmap
            (\ ((day,month),year) ->
               CalendarTime {
                  ctYear = year,
                  ctMonth = month,
                  ctDay = day,
                  ctHour = 0,
                  ctMin = 0,
                  ctSec = 0,
                  ctPicosec = 0,
                  ctWDay = error "Week day not set",
                  ctYDay = error "Year day not set",
                  ctTZName = error "TZNmae not set",
                  ctTZ = 0,
                  ctIsDST = False
                  }
               )
            dmy
   in
      fmap toClockTime calendarForm

-- ----------------------------------------------------------------------
-- Deletion count operations lifted to VersionInfo
-- ----------------------------------------------------------------------

toDeletionCount :: VersionInfo -> WithError Int
toDeletionCount versionInfo 
   = toDeletionCount0 . versionAttributes . user $ versionInfo

upDeleteCount :: VersionInfo -> WithError VersionInfo
upDeleteCount versionInfo0 =
   do
      let
         user0 = user versionInfo0
         versionAttributes0 = versionAttributes user0
      versionAttributes1 <- upDeleteCount0 versionAttributes0
      let
         user1 = user0 {versionAttributes = versionAttributes1}
         versionInfo1 = versionInfo0 {user = user1}
      return versionInfo1

downDeleteCount :: VersionInfo -> WithError VersionInfo
downDeleteCount versionInfo0 =
   do
      let
         user0 = user versionInfo0
         versionAttributes0 = versionAttributes user0
      versionAttributes1 <- downDeleteCount0 versionAttributes0
      let
         user1 = user0 {versionAttributes = versionAttributes1}
         versionInfo1 = versionInfo0 {user = user1}
      return versionInfo1

-- ----------------------------------------------------------------------
-- Deletion count operations on attributes
-- ----------------------------------------------------------------------

toDeletionCount0 :: VersionAttributes -> WithError Int
toDeletionCount0 versionAttributes =
   case lookupVersionAttribute versionAttributes deleteCountKey of
      Nothing -> return 0
      Just str ->
         case readCheck str of
            Just i | i>=0 -> return i
            _ -> fail "Deletion count is not a non-negative integer"

upDeleteCount0 :: VersionAttributes -> WithError VersionAttributes
upDeleteCount0 versionAttributes0 =
   do
      delCount0 <- toDeletionCount0 versionAttributes0
      let
         delCount1 = delCount0 + 1
         versionAttributes1 = 
            setVersionAttribute versionAttributes0 deleteCountKey
               (show delCount1) 
      return versionAttributes1

downDeleteCount0 :: VersionAttributes -> WithError VersionAttributes
downDeleteCount0 versionAttributes0 =
   do
      delCount0 <- toDeletionCount0 versionAttributes0
      if delCount0 == 0
         then
            fail "Object is not deleted"
         else
            done
      let
         delCount1 = delCount0 - 1
         versionAttributes1 = 
            setVersionAttribute versionAttributes0 deleteCountKey
               (show delCount1) 
      return versionAttributes1

deleteCountKey :: String
deleteCountKey = "Deletes"