{- #########################################################################

MODULE        : GUIInteraction
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : User Interaction


   ######################################################################### -}


module GUIInteraction (
        EV,

        GUIObject(..),
        Interactive(..),

        HasEnable(..),
        State(..),

        Reactive(..),
        HasTrigger(..),
        HasMapTrigger(..),
        HasCommand(..),

        reaction,
        handlerGUI,

        GUIEventLoop,
        newGUIEventLoop,

        undefinedEventHandler

) where

import SIM
import Dynamics
import GUIState
import Resources
import GUIObject
import GUIBaseClasses
import Debug(debug)


-- --------------------------------------------------------------------------
-- Reactive GUI Objects 
-- --------------------------------------------------------------------------

class HasTrigger w a => HasCommand w e a where
        command :: (e -> IO a) -> Config (w a)

reaction :: HasCommand w () a => IO a -> Config (w a)
reaction f = command (\() -> f)


-- --------------------------------------------------------------------------
--  Enabling and Disabling of Widgets
-- --------------------------------------------------------------------------

class Interactive w => HasEnable w where
        state      :: State -> Config w
        getState   :: w -> IO State
        disable    :: Config w
        enable     :: Config w
        isEnabled  :: w -> IO Bool
        state s w   = cset w "state" s
        getState w  = cget w "state"
        disable     = state Disabled
        enable      = state Normal
        isEnabled w = do {st <- getState w; return (st /= Disabled)}


-- --------------------------------------------------------------------------
--  GUI Event Loop
-- --------------------------------------------------------------------------

instance GUIObject w => EventHandlerDesignator w GUIOBJECT where
        toHandlerID = return . toGUIObject

type GUIEventLoop = EventLoop GUIOBJECT

newGUIEventLoop :: IO (EventLoop GUIOBJECT)
newGUIEventLoop = newEventLoop

handlerGUI :: (GUIObject (b c), Reactive b c) =>
          EventLoop GUIOBJECT -> IO () -> Config (b c)
handlerGUI el c o = do 
        el # registerEH o (triggered o >>> c) 
        return o



-- --------------------------------------------------------------------------
--  Errors
-- --------------------------------------------------------------------------

undefinedEventHandler :: IOError
undefinedEventHandler = userError "Error handler has not been defined" 
