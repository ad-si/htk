{- This file contains the doMMiSSRegistrations function, which should
   execute all the registrations required in the mmiss directory. 
   -}
module MMiSSRegistrations(doMMiSSRegistrations) where

import MMiSSPaths
import MMiSSObjects
import MMiSSDisplay

doMMiSSRegistrations :: IO ()
doMMiSSRegistrations =
   do
      registerMMiSSPaths
      registerMMiSSObjects
      registerMMiSSDisplay