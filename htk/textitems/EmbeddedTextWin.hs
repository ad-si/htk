-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

---
-- HTk's <strong>embedded windows</strong> inside an editor widget.
module EmbeddedTextWin (

  EmbeddedTextWin,
  createEmbeddedTextWin,

  stretch,
  getStretch

) where

import Core
import Editor
import Frame
import Index
import Computation
import Synchronized
import Resources
import Destructible
import Geometry
import BaseClasses(Widget)
import Wish


-- -----------------------------------------------------------------------
-- type EmbeddedTextWin
-- -----------------------------------------------------------------------

---
-- The <code>EmbeddedTextWin</code> datatype.
newtype EmbeddedTextWin = EmbeddedTextWin GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new embedded window inside an editor widget and returns 
-- a handler.
-- @param ed      - the parent editor widget.
-- @param i       - the editor's index to place the embedded window.
-- @param w       - the contained widget.
-- @param cnf     - the list of configuration options for this embedded
--                  text window.
-- @return result - An embedded window inside an editor widget.
createEmbeddedTextWin :: (HasIndex (Editor a) i BaseIndex, Widget w) =>
                         (Editor a) -> i -> w ->
                         [Config EmbeddedTextWin] -> IO EmbeddedTextWin
createEmbeddedTextWin ed i w cnf =
  do
    binx <- getBaseIndex ed i
    pos <- getBaseIndex ed (binx::BaseIndex)
    nm <- getObjectName (toGUIObject w)
    wid <- createGUIObject (toGUIObject ed)
             (EMBEDDEDTEXTWIN (unparse pos) nm) winMethods
    configure (EmbeddedTextWin wid) cnf
  where unparse :: Position -> GUIVALUE
        unparse (x,y) = toGUIValue (RawData (show x ++ "." ++ show y))


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject EmbeddedTextWin where 
---
-- Internal.
  toGUIObject (EmbeddedTextWin w) = w
---
-- Internal.
  cname _ = "EmbeddedTextWin"

---
-- An embedded text window can be destroyed.
instance Destroyable EmbeddedTextWin where
---
-- Destroys an embedded text window.
  destroy = destroy . toGUIObject

---
-- You can synchronize on an embedded text window object.
instance Synchronized EmbeddedTextWin where
---
-- Synchronizes on an embedded text window object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- widget specific configuration options
-- -----------------------------------------------------------------------

---
-- If set the contained widget is stretched vertically to match the
-- spacing of the line.
stretch :: Toggle -> Config EmbeddedTextWin
stretch t w = cset w "stretch" t

---
-- Gets the current stretch setting.
getStretch :: EmbeddedTextWin -> IO Toggle
getStretch ew = cget ew "stretch"


-- -----------------------------------------------------------------------
-- index
-- -----------------------------------------------------------------------

instance HasIndex (Editor a) EmbeddedTextWin BaseIndex where
  getBaseIndex tp win =
    synchronize win
      (do
         name <- getObjectName (toGUIObject win)
         case name of
           (TextPaneItemName pnm (EmbeddedWindowName wnm)) ->
              do
                str <- evalTclScript (tkWinIndex pnm wnm)
                return (read str))

-- -----------------------------------------------------------------------
-- Text Item Methods
-- -----------------------------------------------------------------------

winMethods = 
  Methods tkGetTextWinConfig
          tkSetTextWinConfigs
          tkCreateTextWin
          (packCmd voidMethods)
          (gridCmd voidMethods)
          (destroyCmd voidMethods)
          (bindCmd voidMethods)
          (unbindCmd voidMethods)
          (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- Unparsing of Text Window Commands
-- -----------------------------------------------------------------------

tkGetTextWinConfig :: ObjectName -> ConfigID -> TclScript
tkGetTextWinConfig (TextPaneItemName name qual) cid =   
        [(show name) ++ " window cget " ++ (show qual) ++ " -" ++ cid]
tkGetTextWinConfig _ _ = []   -- ich bin unschuldig, war so bei Einar!
                              -- TD (ludi), geht überhaupt ??
{-# INLINE tkGetTextWinConfig #-}

tkSetTextWinConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetTextWinConfigs (TextPaneItemName name qual) args = 
  [show name ++ " window configure " ++ show qual ++ " " ++
   showConfigs args]
tkSetTextWinConfigs _ _ = []
{-# INLINE tkSetTextWinConfigs #-}

tkCreateTextWin :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                   [ConfigOption] -> TclScript
tkCreateTextWin _ (EMBEDDEDTEXTWIN pos wid) (TextPaneItemName name qual) _
                confs =
  [show name ++ " window create " ++ show pos ++ " -window " ++ show wid]
{-# INLINE tkCreateTextWin #-}

tkWinIndex :: ObjectName -> ObjectName -> TclScript
tkWinIndex pnm wnm = [show pnm ++ " index " ++ show wnm] 
{-# INLINE tkWinIndex #-}
