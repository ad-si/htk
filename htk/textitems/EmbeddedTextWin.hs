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

newtype EmbeddedTextWin = EmbeddedTextWin GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

createEmbeddedTextWin :: (HasIndex (Editor a) i BaseIndex, Widget w) =>
                         (Editor a) -> i -> w ->
                         [Config EmbeddedTextWin] -> IO EmbeddedTextWin
createEmbeddedTextWin ed i w ol =
  do
    binx <- getBaseIndex ed i
    pos <- getBaseIndex ed (binx::BaseIndex)
    nm <- getObjectName (toGUIObject w)
    wid <- createGUIObject (toGUIObject ed)
             (EMBEDDEDTEXTWIN (unparse pos) nm) winMethods
    configure (EmbeddedTextWin wid) ({-(parent ed) :-} ol)
  where unparse :: Position -> GUIVALUE
        unparse (x,y) = toGUIValue (RawData (show x ++ "." ++ show y))


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject EmbeddedTextWin where 
  toGUIObject (EmbeddedTextWin w) = w
  cname _ = "EmbeddedTextWin"

instance Destroyable EmbeddedTextWin where
  destroy = destroy . toGUIObject

--instance HasPadding EmbeddedTextWin

--instance HasAlign EmbeddedTextWin

{-
instance ParentWidget (Editor a) EmbeddedTextWin where
        parent tp item = do {
                packTextWindowItem (toGUIObject tp) (toGUIObject item) (Just winMethods);
                return item
                }
-}

instance Synchronized EmbeddedTextWin where
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- widget specific configuration options
-- -----------------------------------------------------------------------

stretch :: Toggle -> Config EmbeddedTextWin
stretch t w = cset w "stretch" t

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
