{- #########################################################################

MODULE        : GUIMethods
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines a structure holding the Tk commands that are needed
                to create, destroy and update a specific widget. Each widget
                kind has its own set of Tk methods.

                The desctruction command is for physically deletion of the
                widget on the Tk side, whereas the cleanup command is meant
                to clean up the space (Tk variables etc) allocated by
                the widget.


   ######################################################################### -}


module GUIMethods (
        module GUIEvent,
        module GUIValue,
        module GUIObjectKind,
        module GUIObjectName,

        TclCmd,
        TclScript,
        ConfigOption,
        ConfigID,
        Binding(..),

        Methods(..),

        defMethods,
        voidMethods,

        showConfigs,
        showConfig,

        tkDeclBindings,
        tkDeclVar,
        tkUndeclVar

        ) where


import Object(ObjectID(..))
import Resources
import GUIValue
import GUIEvent
import GUIWish(TclCmd,TclScript)
import GUIObjectKind
import GUIObjectName
import Debug(debug)


-- --------------------------------------------------------------------------
--  Methods
-- --------------------------------------------------------------------------

data Methods = 
        Methods {
          cgetCmd     :: ObjectName -> ConfigID -> TclScript,             
          csetCmd     :: ObjectName -> [ConfigOption] -> TclScript,
          createCmd   :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript, 
          packCmd     :: ObjectKind -> ObjectName -> ObjectName -> 
                        [ConfigOption] -> ObjectID -> [Binding] -> TclScript, 
          destroyCmd  :: ObjectID -> ObjectName -> TclScript,
          cleanupCmd  :: ObjectID -> ObjectName -> TclScript,
          bindCmd     :: ObjectName -> ObjectID -> Binding -> TclScript,
          unbindCmd   :: ObjectName -> ObjectID -> Binding -> TclScript
        }


-- --------------------------------------------------------------------------
-- Configuration Options 
-- --------------------------------------------------------------------------

type ConfigID   = String

type Binding    = (EventPatternID,EventFieldDsgs) 
 
type ConfigOption = (ConfigID, GUIVALUE)


-- --------------------------------------------------------------------------
--  Instances (Show)
-- --------------------------------------------------------------------------

showConfig (cid, cval) = "-" ++ cid ++ " " ++ show cval

showConfigs [] = " "
showConfigs (x : ol) = (showConfig x) ++ " " ++ (showConfigs ol)
        

-- --------------------------------------------------------------------------
--  GUIObject Default Methods (for widgets and foreign objects mainly)
-- --------------------------------------------------------------------------

defMethods :: Methods
defMethods =  Methods
                tkGetWidgetConfig
                tkSetWidgetConfigs
                tkCreateWidget
                tkPackWidget
                tkDestroyWidget
                tkCleanupWidget
                tkBindWidget
                tkUnbindWidget  

voidMethods :: Methods
voidMethods =  Methods
                (\ _ _ -> [])
                (\ _ _ -> [])
                (\ _ _ _ _ -> [])
                (\ _ _ _ _ _ _ -> [])
                (\ _ _ -> [])
                (\ _ _ -> [])
                (\ _ _ _ -> [])
                (\ _ _  _ -> [])        



-- --------------------------------------------------------------------------
-- Unparsing of Widget (default methods) 
-- --------------------------------------------------------------------------

tkCreateWidget :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateWidget kind name oid opts =
        [(show kind) ++ " " ++ (show name) ++ " " ++ (showConfigs opts)]


tkPackWidget :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> 
                        ObjectID -> [Binding] -> [TclCmd]
tkPackWidget _ _ name opts oid binds = 
        ("pack " ++ (show name) ++ " " ++ (showConfigs opts)) :
        tkDeclBindings name oid binds


tkDeclBindings :: ObjectName -> ObjectID -> [Binding] -> TclScript
tkDeclBindings name oid binds = concatMap (tkBindWidget name oid) binds


tkBindWidget :: ObjectName -> ObjectID -> Binding -> TclScript
tkBindWidget wn (ObjectID no) (tkev,fields) = 
        ["bind " ++ show wn ++ " " ++ tkev ++ 
        " {puts stdout {EV " ++ show no ++ " " ++ tkev ++ " " ++
        show fields ++ " }; flush stdout};"]
tkBindWidget _ _ _ = []


tkDestroyWidget :: ObjectID -> ObjectName -> TclScript
tkDestroyWidget oid name = ["destroy " ++ show name]


tkCleanupWidget :: ObjectID -> ObjectName -> TclScript
tkCleanupWidget _ _ = []


tkUnbindWidget :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindWidget wn _ (tkev,_) = ["bind " ++ show wn ++ " " ++ tkev ++ " {}"]


tkGetWidgetConfig :: ObjectName -> ConfigID -> TclScript
tkGetWidgetConfig name cid = [(show name) ++ " cget -" ++ cid]


tkSetWidgetConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetWidgetConfigs _ [] = []
tkSetWidgetConfigs name args = [show name ++ " configure " ++ showConfigs args]


tkDeclVar :: String -> String -> TclScript
tkDeclVar var val = ["global " ++ var, "set " ++ var ++ " " ++ val]


tkUndeclVar :: String -> TclScript
tkUndeclVar var = ["global " ++ var, "unset " ++ var]
