{- #########################################################################

MODULE        : Controller
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A controller for destructible objects.
 
   ######################################################################### -}


module Controller (

        Destructible(..),
        attach,
        controller,
        controller'
        ) where


import Concurrency
import Interaction
import InterActor
import SIMClasses
import Debug(debug)

-- --------------------------------------------------------------------------
--  Controllers for Destructible Objects
-- --------------------------------------------------------------------------

attach :: (Destructible o1, Destructible o2) => o1 -> o2 -> IO ()
attach o1 o2 = do 
   interactor (\iact ->
         destroyed o1 >>> do {destroy o2; stop iact}
      +> destroyed o2 >>> stop iact
      )
   done


controller :: Destructible w => w -> (InterActor -> IA a) -> IO InterActor
controller w ev = newInterActor (\iact -> 
        tryEV (ev iact)    >>>= either handleErr (const done)
   +>   destroyed w        >>> stop iact 
  ) where handleErr e = do {
                h <- getErrorHandler;
                h e
                }
         
controller' :: Destructible w => w -> IA a -> IO InterActor
controller' w e = controller w (\_ -> e)
