{- #########################################################################

MODULE        : GUICore
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The core features of the GUI needed to build the widgets etc.


   ######################################################################### -}


module GUICore (
        EventID,
        EventDesignator(..),

        ObjectID,
        Object(..),

        module Resources,
        module GUIEvent,
        module GUIValue,
        module GUIObject,
        module GUIState,
        module GUIRealise,      
        module GUIBaseClasses,
        module GUIInteraction,
        module Geometry,
        module Colour,
        module Cursor,
        module Font,

        module Window,
        module Packer
        
        ) where

import SIM
import GUIBaseClasses
import GUIInteraction
import Resources
import Geometry
import Colour
import Cursor
import Font
import GUIState
import GUIRealise
import GUIValue
import GUIEvent
import GUIObject
import FiniteMap
import Window
import Packer
import Debug(debug)

