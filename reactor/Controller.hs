{- #########################################################################

MODULE        : Controller
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A controller for destructible objects.

Documented in Einar's thesis, section 7.5.6
 
   ######################################################################### -}


module Controller (
   Destructible(..),
   attach, -- attach o1 o2 
           -- means "when o1 is destroyed, destroy o2".
   controller,
           -- controller can be seen as a more sophisticated way
           -- of setting up and running an interactor attached to
           -- a specified object, in which
           -- (1) destruction messages on the object argument
           --     stop the interactor;
           -- (2) errors in the interaction action raise the
           --     user-registered error handler (defined by Computation)
   controller'
           -- controller' is an abbreviation for controller.
   ) where


import qualified Computation
import Concurrency
import Interaction
import InterActor
import SIMClasses
import Debug(debug)

-- --------------------------------------------------------------------------
--  Controllers for Destructible Objects
-- --------------------------------------------------------------------------

attach :: (Destructible o1, Destructible o2) => o1 -> o2 -> IO ()
attach o1 o2 = 
   do 
      interactor 
         (\iact ->
               destroyed o1 >>> 
                  do 
                     destroy o2
                     stop iact
            +> destroyed o2 >>> stop iact
         )
      done


controller :: Destructible w => w -> (InterActor -> IA a) -> IO InterActor
controller w event = newInterActor (
   \iact -> 
         tryEV (event iact) >>>= either handleErr (const done)
      +> destroyed w >>> stop iact 
   ) 
   where 
      handleErr err = 
         do
            handler <- Computation.getErrorHandler
            handler err
        
controller' :: Destructible w => w -> IA a -> IO InterActor
controller' w e = controller w (\_ -> e)




