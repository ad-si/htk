{- This file contains the doMMiSSRegistrations function, which should
   execute all the registrations required in the mmiss directory. 
   -}
module MMiSSRegistrations(doMMiSSRegistrations) where

import MMiSSPaths

doMMiSSRegistrations :: IO ()
doMMiSSRegistrations =
   do
      registerMMiSSPaths
