{- #########################################################################

MODULE        : ListBox
AUTHOR        : Einar Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : alpha
DESCRIPTION   : Listbox Widget. Fortunately, Haskell and Tk agree on one
                thing: indices start with 0.

TO BE DONE    : Nearest command

                valueSet operation


   ######################################################################### -}


module ListBox (
        module Selection,
        module Index,

        ListBox,
        newListBox,
        
        SelectMode(..),
        selectMode,
        getSelectMode,

        activateElem,
        selectionAnchor,

        ListBoxElem(..),
        elemNotFound

        ) where

import List
import Concurrency

import GUICore
import Packer
import ScrollBar
import Index
import Selection
import Char(isSpace)
import Mouse
import XSelection
import Interaction()
import Debug(debug)

-- --------------------------------------------------------------------------
-- ListBox Type
-- --------------------------------------------------------------------------

newtype ListBox a = ListBox GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- ListBox Creation 
-- --------------------------------------------------------------------------

newListBox :: GUIValue a => [Config (ListBox [a])] -> IO (ListBox [a])
newListBox ol = do
        w <- createGUIObject (LISTBOX []) lboxMethods 
        configure (ListBox w) ol


-- --------------------------------------------------------------------------
-- ListBox Instantiations
-- --------------------------------------------------------------------------

instance GUIObject (ListBox a) where 
        toGUIObject (ListBox w) = w
        cname _ = "ListBox"

instance Destructible (ListBox a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (ListBox a)

instance Widget (ListBox a)

instance ChildWidget (ListBox a)

instance Synchronized (ListBox a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder (ListBox a)

instance HasColour (ListBox a) where 
        legalColourID = hasForeGroundColour

instance HasEnable (ListBox a)

instance HasFont (ListBox a)

instance HasGrid (ListBox a)

instance HasScroller (ListBox a)

instance HasSize (ListBox a)

instance GUIValue a => Variable ListBox [a] where
        setVar lb el = synchronize lb (do {
                setObjectKind (toGUIObject lb) (LISTBOX sel);
                try (execMethod lb (\nm -> tkInsert nm 0 sel));
                done
                }) where sel = map toGUIValue el
        getVar lb = synchronize lb (do {
                kind <- getObjectKind (toGUIObject lb);
                case kind of {(LISTBOX el) -> return (map fromGUIValue el)}
                })
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })


-- --------------------------------------------------------------------------
-- ListBox Configurations
-- --------------------------------------------------------------------------

selectMode :: GUIValue a => SelectMode -> Config (ListBox a)
selectMode sm lbox = cset lbox "selectmode" sm


getSelectMode :: GUIValue a => (ListBox a) -> IO SelectMode 
getSelectMode lbox = cget lbox "selectmode"


-- --------------------------------------------------------------------------
-- BBox 
-- --------------------------------------------------------------------------

instance HasIndex (ListBox a) i Int => HasBBox (ListBox a) i  where
        bbox w i = do {
                binx <- getBaseIndex w i;
                ans <- try (evalMethod w (\nm -> [tkBBox nm (binx::Int)]));
                case ans of
                        (Left e)  -> return Nothing
                        (Right v) -> return (Just v)
                } where tkBBox nm i = show nm ++ " bbox " ++ show i
        

-- --------------------------------------------------------------------------
-- Index
-- --------------------------------------------------------------------------

data Eq a => ListBoxElem a = ListBoxElem a deriving Eq


-- --------------------------------------------------------------------------
-- Has Index
-- --------------------------------------------------------------------------

instance HasIndex (ListBox a) Int Int where
        getBaseIndex lb i = return i


instance HasIndex (ListBox a) EndOfText Int where
        getBaseIndex lb _ = getIndexNumber lb "end"


instance HasIndex (ListBox a) Pixels Int where
        getBaseIndex lb p = getIndexNumber lb (show p)


instance (Eq a,GUIValue a) => HasIndex (ListBox [a]) (ListBoxElem a) Int where 
        getBaseIndex lb (ListBoxElem val) = do
                kind <- getObjectKind (toGUIObject lb)
                case kind of
                        (LISTBOX elems) -> 
                                case findIndex (\e -> show e == val') elems of
                                        Nothing  -> raise elemNotFound
                                        (Just i) -> return i
                 where val' = show (toGUIValue val)


instance (Eq a,GUIValue a) => HasIndex (ListBox [a]) Int (ListBoxElem a) where 
        getBaseIndex lb i = synchronize lb (do {
                elems <- getValue lb;
                if (i >= 0) && (i <= (length elems - 1)) then
                                return (ListBoxElem (elems !! i))
                else
                        raise elemNotFound
                })


getIndexNumber :: ListBox a -> String -> IO Int
getIndexNumber lb i = evalMethod lb (\lnm -> [show lnm ++ " index "  ++ i])


-- --------------------------------------------------------------------------
-- ListBox Selection
-- --------------------------------------------------------------------------

instance HasSelection (ListBox a) where
        clearSelection lb = execMethod lb (\nm -> tkSelectionClearAll nm)


instance (HasIndex (ListBox a) i Int) => HasSelectionIndex (ListBox a) i where
        selection i lb = synchronize lb (do {
                binx <- getBaseIndex lb i;
                execMethod lb (\ nm -> tkSelectionSetItem nm binx);
                return lb
                })
        isSelected lb i = synchronize lb (do {
                binx <- getBaseIndex lb i; 
                evalMethod lb (\nm -> tkSelectionIncludes nm binx)
                })


instance HasSelectionBaseIndex (ListBox a) [Int] where
        getSelection lb = do 
                sel <- evalMethod lb (\ nm -> tkCurSelection nm)
                case (((map read) .words) sel) of
                        []  -> return Nothing
                        l   -> return (Just l)



instance (HasIndex (ListBox a) i1 Int, HasIndex (ListBox a) i2 Int) 
        => HasSelectionIndexRange (ListBox a) i1 i2 
  where
        selectionRange start end lb = synchronize lb (do {
                start' <- getBaseIndex lb start;
                end' <- getBaseIndex lb end;
                execMethod lb (\ nm -> tkSelectionSet nm start' end');
                return lb       
                })

        
instance HasSelectionBaseIndexRange (ListBox a) Int where
        getSelectionStart lb = do {
                sel <- getSelection lb;
                case sel of
                        Nothing       -> return Nothing 
                        (Just (v:_)) -> return (Just v)
                }
        getSelectionEnd lb = do {
                sel <- getSelection lb;
                case sel of
                         Nothing  -> return Nothing
                         (Just l) -> (return . Just . head . reverse) l
                }


-- --------------------------------------------------------------------------
--  Other ListBox Operations 
-- --------------------------------------------------------------------------

activateElem :: HasIndex (ListBox a) i Int => ListBox a -> i -> IO ()
activateElem lb inx  =
        synchronize lb (do {
                binx <- getBaseIndex lb inx; 
                execMethod lb (\ nm -> tkActivate nm binx)
                })


selectionAnchor :: HasIndex (ListBox a) i Int => ListBox a -> i -> IO ()
selectionAnchor lb inx =
        synchronize lb (do {
                binx <- getBaseIndex lb inx; 
                execMethod lb (\nm -> tkSelectionAnchor nm binx);
                done
                })


-- --------------------------------------------------------------------------
--  Selection Event 
-- --------------------------------------------------------------------------

instance (Eq a,GUIValue a) => Reactive ListBox [a] where
        triggered lb = mouseButtonPress lb 1 >>> do {
                msel <- getSelection lb;
                case msel of
                        Nothing -> return []
                        (Just sel) -> do {
                                elems <- mapM (getBaseIndex lb) (sel::[Int]);
                                return (map (\(ListBoxElem e) -> e) elems)
                                }
                }

instance (Eq a,GUIValue a) => HasTrigger ListBox [a] where
        getTrigger = return . triggered


-- --------------------------------------------------------------------------
--  SelectMode 
-- --------------------------------------------------------------------------

data SelectMode = Single | Browse | Multiple | Extended deriving (Eq,Ord,Enum)

instance GUIValue SelectMode where
        cdefault = Single
        
instance Read SelectMode where
   readsPrec p b =
     case dropWhile (isSpace) b of
        's':'i':'n':'g':'l':'e':xs -> [(Single,xs)]
        'b':'r':'o':'w':'s':'e':xs -> [(Browse,xs)]
        'm':'u':'l':'t':'i':'p':'l':'e':xs -> [(Multiple,xs)]
        'e':'x':'t':'e':'n':'d':'e':'d':xs -> [(Extended,xs)]
        _ -> []

instance Show SelectMode where
   showsPrec d p r = 
      (case p of 
         Single -> "single"
         Browse -> "browse"
         Multiple -> "multiple"
         Extended -> "extended"
        ) ++ r


-- --------------------------------------------------------------------------
-- Exceptions
-- --------------------------------------------------------------------------

elemNotFound :: IOError
elemNotFound = userError "listbox element not found"



-- --------------------------------------------------------------------------
-- ListBox Methods
-- --------------------------------------------------------------------------

lboxMethods :: Methods
lboxMethods = defMethods{
                cleanupCmd = tkCleanupListBox,
                createCmd = tkCreateListBox,
                packCmd = packListBox
                }
        where packListBox (LISTBOX el) pn nm cp oid binds = 
                (packCmd defMethods) (LISTBOX el) pn nm cp oid binds ++ 
                tkCreateListBoxElems nm el
              packListBox _ pn nm cp oid binds =
                error "illegal pack command for listbox"


-- --------------------------------------------------------------------------
-- Tk Commands
-- --------------------------------------------------------------------------

tkCreateListBox :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> 
                        TclScript
tkCreateListBox kind name oid confs = 
        tkDeclVar ("sv" ++ show oid) (show name) ++ 
        (createCmd defMethods) kind name oid confs 
{-# INLINE tkCreateListBox #-}


tkCleanupListBox :: ObjectID -> ObjectName -> TclScript
tkCleanupListBox oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupListBox #-}


tkCreateListBoxElems ::  ObjectName -> [GUIVALUE] -> TclScript
tkCreateListBoxElems name elems = 
        [show name ++ " insert 0 " ++ showElements elems]
{-# INLINE tkCreateListBoxElems #-}


showElements :: [GUIVALUE] -> String
showElements = concatMap (++ " ") . (map show) 
{-# INLINE showElements #-}


tkActivate :: ObjectName -> Int -> TclScript
tkActivate name inx = [show name ++ " activate " ++ show inx]
{-# INLINE tkActivate #-}


tkCurSelection :: ObjectName -> TclScript
tkCurSelection name = [show name ++ " curselection "]
{-# INLINE tkCurSelection #-}


tkDelete :: ObjectName -> String -> String -> TclCmd
tkDelete name first last = show name ++ " delete " ++ first ++ " " ++ last
{-# INLINE tkDelete #-}


tkInsert ::  ObjectName -> Int -> [GUIVALUE] -> TclScript
tkInsert name inx elems = 
        [tkDelete name "0" "end",
        show name ++ " insert " ++ show inx ++ " " ++ showElements elems
        ]
{-# INLINE tkInsert #-}


tkSelectionAnchor :: ObjectName -> Int -> TclScript
tkSelectionAnchor name inx = [show name ++ " selection anchor " ++ show inx]  
{-# INLINE tkSelectionAnchor #-}


tkSelectionIncludes :: ObjectName -> Int -> TclScript
tkSelectionIncludes name inx = 
        [show name ++ " selection includes " ++ show inx]  
{-# INLINE tkSelectionIncludes #-}


tkSelectionClear :: ObjectName -> Int -> Int -> TclScript
tkSelectionClear name first last = 
     [show name ++ " selection clear " ++ show first ++ " " ++ show last]
{-# INLINE tkSelectionClear #-}


tkSelectionClearAll :: ObjectName -> TclScript
tkSelectionClearAll name = [show name ++ " selection clear 0 end"]
{-# INLINE tkSelectionClearAll #-}


tkSelectionSet :: ObjectName -> Int -> Int -> TclScript
tkSelectionSet name first last = 
        [show name ++ " selection set " ++ show first ++ " " ++ show last]
{-# INLINE tkSelectionSet #-}


tkSelectionSetItem :: ObjectName -> Int -> TclScript
tkSelectionSetItem name first = 
        [show name ++ " selection set " ++ show first]
{-# INLINE tkSelectionSetItem #-}

