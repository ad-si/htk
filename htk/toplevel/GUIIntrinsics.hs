{- #########################################################################

MODULE        : GUIIntrinsics
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A untyped, fully unsafe, but flexible interface to
                the commands of the GUI.
                


   ######################################################################### -}


module GUIIntrinsics (
        GUIObject(..),
        tkcget,
        pset,
        pget,

        module GUIBaseClasses,
        module GUIObject,
        module GUIObjectKind,

        BitMapDesignator(..),
        BitMapHandle(..),
        HasBitMap(..),
        BitMap,
        Image,
        HasPhoto(..),

        EventDesignator(..),
        EventInfo,

        Widget(..),
        ParentWidget(..),
        HasPadding(..),
        ButtonWidget(..),
        SelectButton(..), 
        HasIndicator(..),
        HasAccelerator(..),


        lookupGUIObject,

        ObjectKind(..), 
        CanvasItemKind(..),
        getObjectKind
        
        ) where

import Thread
import GUICore
import GUIState
import GUIBaseClasses
import Packer
import Image
import BitMap
import Button
import MenuItem
import Indicator
import GUIObject
import GUIObjectKind
import Debug(debug)


-- --------------------------------------------------------------------------
-- Base Classes 
-- --------------------------------------------------------------------------

instance HasColour GUIOBJECT where {legalColourID _ _ = True}
instance HasSize GUIOBJECT
instance HasBorder GUIOBJECT
instance HasFont GUIOBJECT
instance GUIValue v => HasText GUIOBJECT v
instance HasUnderline GUIOBJECT
instance HasJustify GUIOBJECT
instance HasGrid GUIOBJECT
instance HasOrientation GUIOBJECT
instance HasAlign GUIOBJECT


-- --------------------------------------------------------------------------
-- BitMaps and Images 
-- --------------------------------------------------------------------------

instance HasBitMap GUIOBJECT
instance HasPhoto GUIOBJECT

-- --------------------------------------------------------------------------
-- Widgets 
-- --------------------------------------------------------------------------

instance ParentWidget GUIOBJECT GUIOBJECT
instance HasPadding GUIOBJECT

