{- --------------------------------------------------------------------
 -
 - Module: UserInteraction
 -
 - Author: cxl / ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module UserInteraction (

  UIA(..),
  userInteraction
--  liftIA

) where

import RVar
import List
import EventStream
import InterActor
import ExternalEvent
import GUIObject
import GUIState
import Event
import SIMClasses
import Computation

newtype UIA a = UIA (IA a, [GUIOBJECT])

instance Actor InterActor (UIA ()) where
   become f (UIA(ia, ws)) = become f ia 

instance Actor (EventStream a) (UIA a) where
   become f (UIA(ia, ws)) = become f ia

instance Functor UIA where
   fmap f e  = e >>>= return . f

instance Event UIA where
  UIA (ia, ws) +> UIA (ia', ws') =
    UIA (ia +> ia', nub (ws ++ ws'))
  UIA (ia, ws) >>>= a = UIA (ia >>>= a, ws)

userInteraction :: (InterActor -> UIA ()) -> IO ()
userInteraction f =
  interactor (\i -> let UIA (ia, ws) = f i
                    in ia +> stopWhenDestroyed i (newRVar ws) ws)
  where stopWhenDestroyed iact wref (w : ws) =
          case ws of
            [] -> (destroyed w >>> widgetDestroyed wref iact w)
            _  -> (destroyed w >>> widgetDestroyed wref iact w) +>
                    stopWhenDestroyed iact wref ws
        widgetDestroyed wref' iact w =
          do
            wref <- wref'
            ws <- getVar wref
            putStr ("number of widgets = " ++ show(length ws) ++ "...   ")
            (let nuws = delete w ws
             in (if null nuws then stop iact >> putStrLn "Interactor stopped"
                 else setVar wref nuws >> putStr "state updated ...   ") >>
                putStrLn ("widget destroyed, " ++ show (length nuws) ++ " widgets left\n"))
