{- #########################################################################

MODULE        : Menu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Menu. 

TO BE DONE    : For the time being, tearoff must be set at creation time 
                and no later. The problem with tk is that the tearoff
                option changes the identities of the menu items. The
                implementation with variable tearoffs is therefore
                much more complicated as one would like!



   ######################################################################### -}


module Menu (
        Menu,

        newMenu,
        popup,

        post,
        unpost,
        tearOff,

        addMenuItem     -- should be private
        
        ) where


import SIM
import Dynamics
import GUICore
import Image
import Packer
import BitMap
import Debug(debug)

                
-- --------------------------------------------------------------------------
-- Menu
-- --------------------------------------------------------------------------   

data Menu a = Menu GUIOBJECT (PVar (MST a))

type MST a = [IO (IA a)]

        
-- --------------------------------------------------------------------------
-- Menu Creation Command
-- --------------------------------------------------------------------------   

newMenu :: [Config (Menu a)] -> IO (Menu a)
newMenu ol = do
        w <- createGUIObject MENU menuMethods
        pv <- newPVar [return inaction]
        configure (Menu w pv) (tearOff On : ol)


-- --------------------------------------------------------------------------
-- Popup Menu
-- --------------------------------------------------------------------------
        
popup :: (GUIObject i,ParentWidget (Menu a) i) => 
                (Menu a) -> Position -> Maybe i -> IO ()
popup m (x,y) Nothing = 
        execMethod m (\nm -> tkPopup nm x y "")
popup m (x,y) (Just entry) = do {
        name <- getObjectName (toGUIObject entry);
        case name of
                Nothing -> raise objectNotPacked
                (Just (MenuItemName _ i)) ->
                        execMethod m (\nm -> tkPopup nm x y (show i))
                _ -> done
        }


tkPopup :: ObjectName -> Distance -> Distance -> String -> TclScript 
tkPopup wn x y ent = ["tk_popup " ++ show wn ++ " " ++ 
        show x ++ " " ++ show y ++ " " ++ ent]
{-# INLINE tkPopup #-}


        
-- --------------------------------------------------------------------------
-- Menu Instances
-- --------------------------------------------------------------------------   

instance Eq (Menu a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (Menu a) where 
        toGUIObject (Menu w _) = w
        cname _ = "Menu"

instance Destructible (Menu a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Menu a)   

instance Widget (Menu a) where
        fill _ w = return w
        expand _ w = return w

instance Synchronized (Menu a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder (Menu a)

instance HasColour (Menu a) where
        legalColourID w "background" = True
        legalColourID w "foreground" = True
        legalColourID w "activebackground" = True
        legalColourID w "activeforeground" = True
        legalColourID w _ = False

instance HasFont (Menu a)

-- --------------------------------------------------------------------------
-- Events
-- --------------------------------------------------------------------------   

instance HasTrigger Menu a where
        getTrigger (Menu w pv) = do {
                cmd <- getVar pv;
                ev <- sequence cmd;
                return (choose ev)
                }
                                
        
-- --------------------------------------------------------------------------
-- Config Options
-- --------------------------------------------------------------------------   

tearOff :: Toggle -> Config (Menu a)
tearOff tg mn = synchronize mn (do {
        checkIfUnpacked (toGUIObject mn);
        cset mn "tearoff" tg
        })

                
-- --------------------------------------------------------------------------
-- Posting and Unposting Menues
-- --------------------------------------------------------------------------   

post :: (Menu a) -> Position -> IO ()
post mn (x, y) = execMethod mn (\name -> tkPost name x y)

unpost :: (Menu a) -> IO ()
unpost mn = execMethod mn (\name -> tkUnPost name)


-- --------------------------------------------------------------------------
--  Menu Methods
-- --------------------------------------------------------------------------

menuMethods = defMethods{packCmd = tkPackMenu}


-- --------------------------------------------------------------------------
--  Menu Packing
-- --------------------------------------------------------------------------

addMenuItem :: Menu a -> GUIOBJECT -> Methods -> IO (IA a) -> IO ()
addMenuItem mn@(Menu w pv) item meths cmd = do {
        packMenuItem (toGUIObject mn) item (Just meths);
        changeVar' pv (cmd : )
        } 


-- --------------------------------------------------------------------------
--  Unparsing of Menu Commands
-- --------------------------------------------------------------------------

tkPackMenu t (ObjectName "") nm _ oid binds = [] -- toplevel window     
tkPackMenu t (p @ (ObjectName s)) nm co oid binds = -- top menu
        (csetCmd defMethods) p [("menu", (toGUIValue (toWidgetName nm)))]
        ++ tkDeclBindings nm oid binds
tkPackMenu t p nm co oid binds = -- cascade/sub menu
        tkPackCascade t p nm co oid binds ++ tkDeclBindings nm oid binds



tkPackCascade :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> ObjectID -> [Binding] -> [TclCmd]
tkPackCascade _ pname mname _ (ObjectID eno) _ =
        [show pname ++ " entryconfigure " ++ show eno ++ " -menu " ++ show mname]


tkPost :: ObjectName -> Distance -> Distance -> TclScript
tkPost name @ (ObjectName _) x y = [show name ++ " post " ++ show x ++ " " ++ show y]
tkPost name @ (MenuItemName mn i) _ _ = [show mn ++ " postcascade " ++ (show i)]
tkPost _ _ _ = []
{-# INLINE tkPost #-}


tkUnPost :: ObjectName -> TclScript
tkUnPost (MenuItemName _ _) = []
tkUnPost name = [show name ++ " unpost "]
{-# INLINE tkUnPost #-}

