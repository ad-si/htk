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

module TextTag (

  module Index,
        
  TextTag,
  createTextTag,

  addTextTag,
  lowerTextTag,
  raiseTextTag,
  removeTextTag,

  lmargin1,
  getLmargin1,

  lmargin2,
  getLmargin2,

  rmargin,
  getRmargin,

  offset,
  getOffset,

  overstrike,
  getOverstrike,

  underlined,
  getUnderlined,

  bgstipple,
  getBgstipple,

  fgstipple,
  getFgstipple

) where

import Core
import Resources
import Colour
import Font
import Geometry
import Configuration
import Editor
import BitMap
import Selection
import Index
import Computation
import Synchronized
import Destructible
import Tooltip


-- -----------------------------------------------------------------------
-- TextTag type
-- -----------------------------------------------------------------------

data TextTag a = TextTag (Editor a) GUIOBJECT


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

createTextTag :: (HasIndex (Editor a) i1 BaseIndex,
                  HasIndex (Editor a) i2 BaseIndex) =>
                 Editor a -> i1 -> i2 -> [Config (TextTag a)] ->
                 IO (TextTag a)
createTextTag ed i1 i2 ol =
  do
    bi1 <- getBaseIndex ed i1
    bi2 <- getBaseIndex ed i2
    w <- createGUIObject (toGUIObject ed)
                         (TEXTTAG (map unparse [bi1::BaseIndex,bi2]))
                         tagMethods
    configure (TextTag ed w) ol
  where unparse (IndexNo _)   = GUIVALUE HaskellTk ""
        unparse (IndexText s) = GUIVALUE HaskellTk  ("{" ++ s ++ "}")
--        unparse (IndexText s) = GUIVALUE HaskellTk  ("\\{" ++ s ++ "\\}")
        unparse p             = GUIVALUE HaskellTk  (show p ++ " ")


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq (TextTag a) where 
  (TextTag _ w1) == (TextTag _ w2) = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (TextTag a) where 
  toGUIObject (TextTag _ w) = w
  cname _ = "TextTag"

instance Destroyable (TextTag a) where
  destroy = destroy . toGUIObject

instance HasBorder (TextTag a)

instance HasColour (TextTag a) where 
  legalColourID = hasForeGroundColour

instance HasFont (TextTag a)

instance HasJustify (TextTag a)

instance HasLineSpacing (TextTag a)

instance HasTabulators (TextTag a)

instance Synchronized (TextTag a) where
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- Tag Commands
-- -----------------------------------------------------------------------

addTextTag :: (HasIndex (Editor a) i1 BaseIndex,
               HasIndex (Editor a) i2 BaseIndex) =>
              TextTag a -> i1 -> i2 -> IO ()
addTextTag tag @ (TextTag tp _) start end =
  synchronize tag (
    do
      start' <- getBaseIndex tp start
      end' <- getBaseIndex tp end
      execMethod tag (\nm -> tkTagAdd nm start' end')
  )

removeTextTag :: (HasIndex (Editor a) i1 BaseIndex,
                  HasIndex (Editor a) i2 BaseIndex) =>
                 TextTag a -> i1 -> i2 -> IO ()
removeTextTag tag @ (TextTag tp _) start end = 
  synchronize tag (
    do
      start' <- getBaseIndex tp start
      end' <- getBaseIndex tp end
      execMethod tag (\nm -> tkTagRemove nm start' end')
  )

lowerTextTag :: TextTag a -> IO ()
lowerTextTag tag = execMethod tag (\nm -> tkTagLower nm)

raiseTextTag :: TextTag a -> IO ()
raiseTextTag tag = execMethod tag (\nm -> tkTagRaise nm)

        
-- -----------------------------------------------------------------------
-- tag configure options
-- -----------------------------------------------------------------------

lmargin1 :: Distance -> Config (TextTag a)
lmargin1 s tag = cset tag "lmargin1" s

getLmargin1 :: TextTag a -> IO Distance
getLmargin1 tag = cget tag "lmargin1"

lmargin2 :: Distance -> Config (TextTag a)
lmargin2 s tag = cset tag "lmargin2" s

getLmargin2 :: TextTag a -> IO Distance
getLmargin2 tag = cget tag "lmargin2"

rmargin :: Distance -> Config (TextTag a)
rmargin s tag = cset tag "rmargin" s

getRmargin :: TextTag a -> IO Distance
getRmargin tag = cget tag "rmargin"

offset :: Distance -> Config (TextTag a)
offset s tag = cset tag "offset" s

getOffset :: TextTag a -> IO Distance
getOffset tag = cget tag "offset"

overstrike :: Toggle -> Config (TextTag a)
overstrike s tag = cset tag "overstrike" s

getOverstrike :: TextTag a -> IO Toggle
getOverstrike tag = cget tag "overstrike"

underlined :: Toggle -> Config (TextTag a)
underlined s tag = cset tag "underline" s

getUnderlined :: TextTag a -> IO Toggle
getUnderlined tag = cget tag "underline"

bgstipple :: BitMapHandle -> Config (TextTag a)
bgstipple s tag = setBitMapHandle tag "bgstipple" s False

getBgstipple ::TextTag a -> IO BitMapHandle
getBgstipple tag = getBitMapHandle tag "bgstipple"

fgstipple :: BitMapHandle -> Config (TextTag a)
fgstipple s tag = setBitMapHandle tag "fgstipple" s False

getFgstipple :: (TextTag a) -> IO BitMapHandle
getFgstipple tag = getBitMapHandle tag "fgstipple"


-- -----------------------------------------------------------------------
-- Index: Tag First and Last
-- -----------------------------------------------------------------------

instance HasIndex (Editor a) (TextTag a, First) BaseIndex where
  getBaseIndex tp (tag,_) =
    synchronize tag (
      do
        (pnm, tnm) <- getTagName tag
        return (IndexText (show tnm ++ ".first"))
    )

instance HasIndex (Editor a) (TextTag a, Last) BaseIndex where
  getBaseIndex tp (tag,_) =
    synchronize tag (
      do
        (pnm, tnm) <- getTagName tag
        return (IndexText (show tnm ++ ".last"))
    )

getTagName :: GUIObject w => w -> IO (ObjectName, TextItemName)
getTagName tag =
  do
    TextPaneItemName pnm tid <- getObjectName (toGUIObject tag)
    return (pnm, tid)


-- -----------------------------------------------------------------------
-- tag methods
-- -----------------------------------------------------------------------

tagMethods = 
        Methods
                tkGetTextTagConfig
                tkSetTextTagConfigs
                tkCreateTextTag
                (packCmd voidMethods)
                (gridCmd voidMethods)
                tkTagDelete
                tkBindTextTag
                tkUnbindTextTag
                (cleanupCmd defMethods)



-- -----------------------------------------------------------------------
-- unparsing of tag commands
-- -----------------------------------------------------------------------

tkGetTextTagConfig :: ObjectName -> ConfigID -> TclScript
tkGetTextTagConfig (TextPaneItemName name tnm) cid =
  [(show name) ++ " tag cget " ++ (show tnm) ++ " -" ++ cid]
tkGetTextTagConfig _ _ = []
{-# INLINE tkGetTextTagConfig #-}

tkSetTextTagConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetTextTagConfigs _ [] = []
tkSetTextTagConfigs (TextPaneItemName name (tnm @ (TextTagID k))) args = 
  [show name ++ " tag configure " ++ show tnm ++ " " ++ showConfigs args]
tkSetTextTagConfigs _ _ = []
{-# INLINE tkSetTextTagConfigs #-}

tkCreateTextTag :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                   [ConfigOption] -> TclScript
tkCreateTextTag _ (TEXTTAG il) (TextPaneItemName name tnm) _ args =
  [show name ++ " tag add " ++ show tnm ++ " " ++
   concat (map unfold il) ++ " " ++ (showConfigs args)]
  where unfold (GUIVALUE _ str) = str ++ " "
--tkCreateTextTag _ _ _ _ _ = []
{-# INLINE tkCreateTextTag #-}

tkTagAdd :: ObjectName -> BaseIndex -> BaseIndex -> TclScript
tkTagAdd (TextPaneItemName name tnm) start end =
  [show name ++ " tag add " ++ show tnm ++ " " ++ show start ++ " " ++
   show end]
tkTagAdd _ _ _ = []
{-# INLINE tkTagAdd #-}

tkTagDelete :: ObjectName -> TclScript
tkTagDelete (TextPaneItemName name tnm) =
  [show name ++ " tag delete " ++ show tnm]
tkTagDelete _ = []
{-# INLINE tkTagDelete #-}

tkBindTextTag :: ObjectName -> BindTag -> [WishEvent] -> EventInfoSet ->
                 Bool -> TclScript
tkBindTextTag (TextPaneItemName name tnm) bindTag wishEvents
              eventInfoSet _ =
  let doBind = show name ++ " tag bind " ++ show tnm ++ " " ++
               delimitString (foldr (\ event soFar -> showP event soFar)
                                    "" wishEvents) ++ " " ++
               mkBoundCmdArg bindTag eventInfoSet
  in [doBind]
{-# INLINE tkBindTextTag #-}

tkUnbindTextTag :: ObjectName -> BindTag -> [WishEvent] -> Bool ->
                   TclScript
tkUnbindTextTag (TextPaneItemName name tnm) bindTag wishEvents _ =
 [show name ++ " tag bind " ++ show tnm ++ " " ++
  delimitString (foldr (\ event soFar -> showP event soFar)
                       "" wishEvents) ++ " {}"]
{-# INLINE tkUnbindTextTag #-}

tkTagLower :: ObjectName -> TclScript
tkTagLower (TextPaneItemName name tnm) =
  [show name ++ " tag lower " ++ show tnm]
tkTagLower _ = []
{-# INLINE tkTagLower #-}

tkTagRaise :: ObjectName -> TclScript
tkTagRaise (TextPaneItemName name tnm) =
  [show name ++ " tag raise " ++ show tnm]
tkTagRaise _ = []
{-# INLINE tkTagRaise #-}

tkTagRemove :: ObjectName -> BaseIndex -> BaseIndex -> TclScript
tkTagRemove (TextPaneItemName name tnm) start end =
  [show name ++ " tag remove " ++ show tnm ++ " " ++ show start ++ " " ++
   show end]
tkTagRemove _ _ _ = []
{-# INLINE tkTagRemove #-}
