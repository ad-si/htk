{- #########################################################################

MODULE        : Message
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 

   ######################################################################### -}


module Message (
        Message,

        newMessage,

        aspect,
        getAspect

        ) where

import Concurrency
import GUICore
import Packer
import Debug(debug)


-- --------------------------------------------------------------------------
-- Type Message 
-- --------------------------------------------------------------------------           
newtype Message a = Message GUIOBJECT deriving Eq

-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newMessage :: GUIValue a => [Config (Message a)] -> IO (Message a)
newMessage ol = do 
        w <- createWidget MESSAGE;
        configure (Message w) [aspect 150]
        configure (Message w) ol

-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance GUIObject (Message a) where 
        toGUIObject (Message w) = w
        cname _ = "Message"

instance Destructible (Message a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Message a)

instance Widget (Message a) 

instance ChildWidget (Message a) 

instance Synchronized (Message a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder (Message a)

instance HasColour (Message a) where
        legalColourID = hasForeGroundColour

instance HasFont (Message a)

instance HasJustify (Message a)

instance HasPadding (Message a)

instance HasSize (Message a) where
        height _ w = return w
        getHeight _ = return 1

instance GUIValue a => Variable Message a where
        setVar w t = cset w "text" t >> done
        getVar w   = cget w "text"
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

instance GUIValue b => HasText (Message a) b where -- only for the brave
        text t w   = cset w "text" t
        getText w  = cget w "text"



-- --------------------------------------------------------------------------
-- Configuration Configs 
-- --------------------------------------------------------------------------           
aspect :: Int -> Config (Message a)
aspect i w = cset w "aspect" i

getAspect :: Message a -> IO Int
getAspect w = cget w "aspect"
