{- #########################################################################

MODULE        : EmbeddedTextWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Embedded Text Widget Windows

TO BE DONE    : The destroyed event will probably not work work with
                the current definition. Probably, some extra Tk interaction
                is required to handle this case.


   ######################################################################### -}


module EmbeddedTextWin (
        EmbeddedTextWin,
        newEmbeddedTextWin,

        stretch,
        getStretch
        ) where

import Concurrency
import GUICore
import Editor
import Index
import Debug(debug)

-- --------------------------------------------------------------------------
-- Window Items
-- --------------------------------------------------------------------------

newtype EmbeddedTextWin = EmbeddedTextWin GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newEmbeddedTextWin :: (HasIndex (Editor a) i BaseIndex, Widget w)
                 => (Editor a) -> w -> i -> [Config EmbeddedTextWin] -> IO EmbeddedTextWin
newEmbeddedTextWin tp w i ol = do {
        binx <- getBaseIndex tp i;
        pos <- getBaseIndex tp (binx::BaseIndex);
        wid <- createGUIObject (EMBEDDEDTEXTWIN (unparse pos)) winMethods;
        makeChildObject wid (toGUIObject w);
        configure (EmbeddedTextWin wid) ((parent tp) :ol);
} where unparse :: Position -> GUIVALUE
        unparse (x,y) = toGUIValue (RawData (show x ++ "." ++ show y))


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject EmbeddedTextWin where 
        toGUIObject (EmbeddedTextWin w) = w
        cname _ = "EmbeddedTextWin"

instance Destructible EmbeddedTextWin where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject             -- TBD

instance Interactive EmbeddedTextWin

instance HasPadding EmbeddedTextWin

instance HasAlign EmbeddedTextWin

instance ParentWidget (Editor a) EmbeddedTextWin where
        parent tp item = do {
                packTextWindowItem (toGUIObject tp) (toGUIObject item) (Just winMethods);
                return item
                }


instance Synchronized EmbeddedTextWin where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
-- Configuration Options
-- --------------------------------------------------------------------------

stretch :: Toggle -> Config EmbeddedTextWin
stretch t w = cset w "stretch" t

getStretch :: EmbeddedTextWin -> IO Toggle
getStretch ew = cget ew "stretch"


-- --------------------------------------------------------------------------
-- Index 
-- --------------------------------------------------------------------------

instance HasIndex (Editor a) EmbeddedTextWin BaseIndex where
        getBaseIndex tp win = synchronize win (do {
                name <- getObjectName (toGUIObject win);
                case name of
                        (Just (TextPaneItemName pnm (EmbeddedWindowName wnm))) -> 
                                evalTclScript (tkWinIndex pnm wnm)
                        _ -> raise objectNotPacked
                })


-- --------------------------------------------------------------------------
-- Text Item Methods 
-- --------------------------------------------------------------------------

winMethods = 
        Methods
                tkGetTextWinConfig
                tkSetTextWinConfigs
                tkCreateTextWin
                tkPackTextWin
                tkDestroyTextWin
                tkCleanupTextWin
                tkBindTextWin
                tkUnbindTextWin


-- --------------------------------------------------------------------------
-- Unparsing of Text Window Commands 
-- --------------------------------------------------------------------------

tkGetTextWinConfig :: ObjectName -> ConfigID -> TclScript
tkGetTextWinConfig (TextPaneItemName name qual) cid =   
        [(show name) ++ " window cget " ++ (show qual) ++ " -" ++ cid]
tkGetTextWinConfig _ _ = []


tkSetTextWinConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetTextWinConfigs (TextPaneItemName name qual) args = 
        [show name ++ " window configure " ++ show qual ++ " " ++ showConfigs args]
tkSetTextWinConfigs _ _ = []


tkCreateTextWin :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateTextWin (EMBEDDEDTEXTWIN pos) (TextPaneItemName name qual) _ confs =
         [(show name) ++ " window create " ++ (show qual)]


tkPackTextWin :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> 
                ObjectID -> [Binding] -> TclScript
tkPackTextWin _ _ name _ oid binds = []



tkBindTextWin :: ObjectName -> ObjectID -> Binding -> TclScript
tkBindTextWin _ _ _ = []
{-# INLINE tkBindTextWin #-}


tkUnbindTextWin :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindTextWin wn _ _ = []
{-# INLINE tkUnbindTextWin #-}


tkDestroyTextWin :: ObjectID -> ObjectName -> TclScript
tkDestroyTextWin oid name = []


tkCleanupTextWin :: ObjectID -> ObjectName -> TclScript
tkCleanupTextWin _ _ = []
        

tkWinIndex :: ObjectName -> ObjectName -> TclScript
tkWinIndex pnm wnm = [show pnm ++ " index " ++ show wnm] 
{-# INLINE tkWinIndex #-}

