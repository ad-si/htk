{- #########################################################################

MODULE        : HTk
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Haskell Tk.


   ######################################################################### -}


module HTk (
        HTk,
        GUIOBJECT,

        module Computation,
        module Event,
        module InterActor,
        module Interaction,
        module EventStream,

        module EventLoop,
        module GUIBaseClasses,
        module GUIInteraction,
        module Resources,
        module GUIValue,
        module GUIEvent,
        module Geometry,
        module Colour,
        module Window,
        module Packer,
        module Geometry,

        module Icon,
        module Box,

        Menu,
         
        objectNotPacked,
        parentNotPacked,
        objectAlreadyPacked,

        WidgetName,

        GUI,
        Tool(..),
        SingleInstanceTool(..),
        Object(..),
        Synchronized(..),

        htk,

        updateAllTasks,
        updateIdleTasks, 

        Destructible(..),
        controller,
        controller',
        attach,

	shutdown
        
        ) where

import SIM
import Computation

import Event
import EventLoop
import InterActor
import Interaction
import EventStream

import SIM(controller, controller', Destructible(..), attach)

import GUIBaseClasses
import GUIInteraction
import Window
import Packer
import Geometry
import Resources
import Geometry
import Colour
import GUIValue
import GUIEvent
import GUIObject(objectAlreadyPacked, WidgetName,Object(..))
import GUIState(GUIOBJECT)
import GUIRealise(objectNotPacked,parentNotPacked)

import GUICore
import Menu(Menu)
import Icon
import Box
import Debug(debug)

import InfoBus(shutdown)


-- --------------------------------------------------------------------------
-- Type
-- --------------------------------------------------------------------------
                                
type HTk = GUI


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

htk :: [Config HTk] -> IO HTk
htk confs =  do {
        gui <- getToolInstance;
        ses <- configure gui confs;
        return ses
        } 

-- --------------------------------------------------------------------------
-- Application updates
-- --------------------------------------------------------------------------

updateAllTasks :: IO ()
updateAllTasks = execTclScript ["update"]


updateIdleTasks :: IO ()
updateIdleTasks = execTclScript ["update idletasks"]


-- --------------------------------------------------------------------------
-- Application Name
-- --------------------------------------------------------------------------

instance GUIValue v => HasText GUI v where
        text aname gui = do {
                execTclScript ["tk appname " ++ show aname];
                return gui
                }
        getText _ = evalTclScript ["tk appname"]
