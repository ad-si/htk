{- #########################################################################

MODULE        : Entry
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget

TO BE DONE    : insert
                selection adjust


   ######################################################################### -}


module Entry (
        module Selection,
        module Index,
        module ICursor,

        Entry,

        newEntry,

        XCoord(..),

        showText,
        getShowText

        ) where

import Concurrency
import GUICore
import GUIValue
import Packer
import ScrollBar
import Index
import Selection
import XSelection
import ICursor
import Debug(debug)


-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

newtype Entry a = Entry GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Creation
-- --------------------------------------------------------------------------

newEntry :: GUIValue a => [Config (Entry a)] -> IO (Entry a)
newEntry ol = do
        wid <- createGUIObject ENTRY entryMethods
        cset (Entry wid) "textvariable" (tkDeclEntryVar wid)
        configure (Entry wid) ol


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance GUIObject (Entry a) where 
        toGUIObject (Entry  w) = w
        cname _ = "Entry"

instance Destructible (Entry a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Entry a)

instance Widget (Entry a)

instance ChildWidget (Entry a)

instance HasBorder (Entry a)

instance HasColour (Entry a) where
        legalColourID = hasForeGroundColour

instance HasSize (Entry a) where
        height _ w = return w
        getHeight w = return 1

instance HasFont (Entry a) 

instance GUIValue a => Variable Entry a where
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        setVar w  v = synchronize w (
                setTclVariable ((tvarname . objectID . toGUIObject) w) v)
        getVar w   = getTclVariable ((tvarname . objectID . toGUIObject) w)

instance HasJustify (Entry a)

instance HasEnable(Entry a)

instance HasScroller (Entry a) where
        isWfOrientation _ Horizontal = True
        isWfOrientation _ Vertical   = False

instance Synchronized (Entry a) where
        synchronize w = synchronize (toGUIObject w)



-- --------------------------------------------------------------------------
-- Index
-- --------------------------------------------------------------------------

data XCoord = XCoord Distance

instance Show XCoord where
   showsPrec d (XCoord x) r = "@"++show x ++ r


-- --------------------------------------------------------------------------
-- Has Index
-- --------------------------------------------------------------------------

instance HasIndex (Entry a) Int BaseIndex where
        getBaseIndex w i = return (IndexNo i)

instance HasIndex (Entry a) BaseIndex BaseIndex where
        getBaseIndex w i = return i

instance HasIndex (Entry a) EndOfText BaseIndex where
        getBaseIndex w _ = return (IndexText "end")

instance HasIndex (Entry a) XCoord BaseIndex where
        getBaseIndex ent i = return (IndexText (show i))
        
instance HasIndex (Entry a) (ICursor (Entry a)) BaseIndex where
        getBaseIndex ent i = return (IndexText "insert")

instance HasIndex (Entry a) (Selection (Entry a),First) BaseIndex where
        getBaseIndex ent i = return (IndexText "sel.first")

instance HasIndex (Entry a) (Selection (Entry a),Last) BaseIndex where
        getBaseIndex ent i = return (IndexText "sel.last")

instance HasIndex (Entry a) i BaseIndex => HasIndex (Entry a) i Int where
        getBaseIndex w i = do {
                bi <- getBaseIndex w i;
                evalMethod w (\nm -> tkGetIndexNumber nm bi)
         }where tkGetIndexNumber :: ObjectName -> BaseIndex -> TclScript
                tkGetIndexNumber nm bi = [show nm ++ " index " ++ show bi]


-- --------------------------------------------------------------------------
-- Selection
-- --------------------------------------------------------------------------

instance HasSelection (Entry a) where
        clearSelection ent    = 
                execMethod ent (\nm -> [show nm ++ " selection clear"])
                

instance HasIndex (Entry a) i BaseIndex => HasSelectionIndex (Entry a) i where
        selection inx ent = synchronize ent (do {
                binx <- getBaseIndex ent inx;
                execMethod ent (\nm -> [tkSelection nm binx]);
                return ent
                })
        isSelected ent inx = synchronize ent (do {
                binx <- getBaseIndex ent inx;
                start <- getSelectionStart ent;
                end <- getSelectionEnd ent;
                case (start,end,binx) of
                        ((Just start),(Just end),(IndexNo i)) ->
                                return ((start <= i) && (i < end)) 
                        _ ->    return False
                })


instance HasSelectionBaseIndex (Entry a) (Int,Int) where
        getSelection = getSelectionRange
                

instance (HasIndex (Entry a) i1 BaseIndex, HasIndex (Entry a) i2 BaseIndex) 
        => HasSelectionIndexRange (Entry a) i1 i2 
  where
        selectionRange start end ent = synchronize ent (do {
                start' <- getBaseIndex ent start;
                end' <- getBaseIndex ent end;
                execMethod ent (\nm -> [tkSelectionRange nm start' end']);
                return ent
                })



instance HasSelectionBaseIndexRange (Entry a) Int where
        getSelectionStart ent = do {
                mstart <- try (evalMethod ent (\nm -> [show nm ++ " index sel.first "]));
                case mstart of
                        (Left e)  -> return Nothing     -- actually a tk error
                        (Right v) -> return (Just v)
                }
        getSelectionEnd ent = do {
                mend <- try (evalMethod ent (\nm -> [show nm ++ " index sel.last "]));
                case mend of
                         (Left e)  -> return Nothing    -- actually a tk error
                         (Right v) -> return (Just v)
                }


instance HasXSelection (Entry a)                


-- --------------------------------------------------------------------------
-- Insertion Cursor
-- --------------------------------------------------------------------------

instance HasInsertionCursor (Entry a)

instance HasIndex (Entry a) i BaseIndex => 
        HasInsertionCursorIndexSet (Entry a) i 
  where
        insertionCursor inx ent = synchronize ent (do {
                binx <- getBaseIndex ent inx;
                execMethod ent (\nm -> [tkSetInsert nm binx]);
                return ent
                })


instance HasInsertionCursorIndexGet (Entry a) Int where
        getInsertionCursor ent = evalMethod ent (\nm -> [tkGetInsert nm])


-- --------------------------------------------------------------------------
-- Configuration Options
-- --------------------------------------------------------------------------

showText :: GUIValue a => Char -> Config (Entry a)      -- option show
showText ch w = cset w "show" [ch]

getShowText :: GUIValue a => Entry a -> IO Char
getShowText w = do {l <- cget w "show"; return (head (l ++ " "))}


-- --------------------------------------------------------------------------
-- Entry Methods
-- --------------------------------------------------------------------------

entryMethods = defMethods {
                cleanupCmd = tkCleanupEntry,
                createCmd = tkCreateEntry}


-- --------------------------------------------------------------------------
-- Unparsing of Tk Commands
-- --------------------------------------------------------------------------

tvarname :: ObjectID -> String
tvarname oid = "v" ++ (show oid)

tkDeclEntryVar :: GUIOBJECT -> WidgetName
tkDeclEntryVar = WidgetName . tvarname . objectID 
        
tkSetInsert :: ObjectName -> BaseIndex -> TclCmd
tkSetInsert wn i = show wn ++ " icursor " ++ show i
{-# INLINE tkSetInsert #-}

tkGetInsert :: ObjectName -> TclCmd
tkGetInsert wn = show wn ++ " index insert"
{-# INLINE tkGetInsert #-}

tkSelection :: ObjectName -> BaseIndex -> TclCmd
tkSelection wn (IndexNo i) = show wn ++ " selection range " ++ show i ++
        " " ++ show (i + 1)
tkSelection wn _ = show wn ++ " selection range end end"
{-# INLINE tkSelection #-}


tkSelectionRange :: ObjectName -> BaseIndex ->  BaseIndex -> TclCmd
tkSelectionRange wn start end = show wn ++ " selection range " ++ 
        show start ++ " " ++ show end
{-# INLINE tkSelectionRange #-}


tkCreateEntry :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> 
                        TclScript
tkCreateEntry kind name oid confs = 
        tkDeclVar ("sv" ++ show oid) (show name) ++ 
        (createCmd defMethods) kind name oid confs 


tkCleanupEntry :: ObjectID -> ObjectName -> TclScript
tkCleanupEntry oid _ = 
        tkUndeclVar (tvarname oid) ++ tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupEntry #-}

