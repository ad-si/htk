-- | This module does some error-reporting, allowing errors to be replaced
-- by a null value ("REPORTED") indicating that they have already been reported.
module ErrorReporting(
   pairWithErrorCheckReported,
      -- :: WithError a -> WithError b -> WithError (a,b)
      -- combine two errors, eliminating reported errors.
   listWithErrorCheckReported,
      -- :: [WithError a] -> WithError [a]
      -- combine list of errors, ditto.
   reportedError,
      -- :: WithError
      -- ditto
   reported,
      -- :: String
      -- the null error
   ) where

import Computation

pairWithErrorCheckReported :: WithError a -> WithError b -> WithError (a,b)
pairWithErrorCheckReported aWE bWE =
   case (fromWithError aWE,fromWithError bWE) of
      (Right a,Right b) -> hasValue (a,b)
      (Left aMess,Right b) -> hasError aMess
      (Right a,Left bMess) -> hasError bMess
      (Left aMess,Left bMess) ->
         hasError (
            case (aMess == reported,bMess == reported) of
               (False,False) -> aMess ++ "\n" ++ bMess
               (False,True) -> aMess
               (True,False) -> bMess
               (True,True) -> reported
               )

listWithErrorCheckReported :: [WithError a] -> WithError [a]
listWithErrorCheckReported awes =
   foldr
      (\ awe awes ->
         mapWithError
            (\ (a,as) -> a:as)
            (pairWithErrorCheckReported awe awes)
         )
      (hasValue [])
      awes


reportedError :: WithError a
reportedError = hasError reported

reported :: String
reported = "REPORTED"

