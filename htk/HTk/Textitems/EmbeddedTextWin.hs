{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HTk\'s /embedded windows/ inside an editor widget.
module HTk.Textitems.EmbeddedTextWin (

  EmbeddedTextWin,
  createEmbeddedTextWin,

  stretch,
  getStretch

) where

import HTk.Kernel.Core
import HTk.Widgets.Editor
import HTk.Components.Index
import Util.Computation
import Events.Synchronized
import HTk.Kernel.Resources
import Events.Destructible
import HTk.Kernel.Geometry
import HTk.Kernel.BaseClasses(Widget)

-- -----------------------------------------------------------------------
-- type EmbeddedTextWin
-- -----------------------------------------------------------------------

-- | The @EmbeddedTextWin@ datatype.
newtype EmbeddedTextWin = EmbeddedTextWin GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new embedded window inside an editor widget and returns
-- a handler.
createEmbeddedTextWin :: (HasIndex Editor i BaseIndex, Widget w) =>
   Editor
   -- ^ the parent editor widget.
   -> i
   -- ^ the editor\'s index to place the embedded window.
   -> w
   -- ^ the contained widget.
   ->
   [Config EmbeddedTextWin]
   -- ^ the list of configuration options for this embedded
   -- text window.
   -> IO EmbeddedTextWin
   -- ^ An embedded window inside an editor widget.
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

-- | Internal.
instance GUIObject EmbeddedTextWin where
  toGUIObject (EmbeddedTextWin w) = w
  cname _ = "EmbeddedTextWin"

-- | An embedded text window can be destroyed.
instance Destroyable EmbeddedTextWin where
  -- Destroys an embedded text window.
  destroy = destroy . toGUIObject

-- | You can synchronize on an embedded text window object.
instance Synchronized EmbeddedTextWin where
  -- Synchronizes on an embedded text window object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- widget specific configuration options
-- -----------------------------------------------------------------------

-- | If set the contained widget is stretched vertically to match the
-- spacing of the line.
stretch :: Toggle -> Config EmbeddedTextWin
stretch t w = cset w "stretch" t

-- | Gets the current stretch setting.
getStretch :: EmbeddedTextWin -> IO Toggle
getStretch ew = cget ew "stretch"


-- -----------------------------------------------------------------------
-- index
-- -----------------------------------------------------------------------

-- | Internal.
instance HasIndex Editor EmbeddedTextWin BaseIndex where
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
        [show name ++ " window cget " ++ show qual ++ " -" ++ cid]
tkGetTextWinConfig _ _ = []   -- unclear case
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
