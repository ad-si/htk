{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides access to text tags inside an editor widget.
module HTk.Textitems.TextTag (
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

import HTk.Kernel.Core
import HTk.Kernel.Resources
import HTk.Kernel.Geometry
import HTk.Kernel.Configuration
import HTk.Widgets.Editor
import HTk.Components.BitMap
import HTk.Components.Index
import Util.Computation
import Events.Synchronized
import Events.Destructible

-- -----------------------------------------------------------------------
-- TextTag type
-- -----------------------------------------------------------------------

-- | The @TextTag@ datatype.
data TextTag = TextTag Editor GUIOBJECT


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Creates a text tag inside an editor widget and returns a handler.
createTextTag :: (HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex) =>
   Editor
   -- ^ the concerned editor widget.
   -> i1
   -- ^ the start index.
   -> i2
   -- ^ the end index.
   -> [Config TextTag]
   -- ^ the list of configuration options for this text tag.
   -> IO TextTag
   -- ^ A text tag.
createTextTag ed i1 i2 cnf =
  do
    bi1 <- getBaseIndex ed i1
    bi2 <- getBaseIndex ed i2
    w <- createGUIObject (toGUIObject ed)
                         (TEXTTAG (map unparse [bi1::BaseIndex,bi2]))
                         tagMethods
    configure (TextTag ed w) cnf
  where unparse (IndexNo _)   = GUIVALUE HaskellTk ""
        unparse (IndexText s) = GUIVALUE HaskellTk  ("{" ++ s ++ "}")
        unparse p             = GUIVALUE HaskellTk  (show p ++ " ")


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq TextTag where
  (TextTag _ w1) == (TextTag _ w2) = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject TextTag where
  toGUIObject (TextTag _ w) = w
  cname _ = "TextTag"

-- | A text tag can be destroyed.
instance Destroyable TextTag where
  -- Destroys a text tag.
  destroy = destroy . toGUIObject

-- | A text tag has a configureable border.
instance HasBorder TextTag

-- | A text tag has a configureable foregroud and background colour.
instance HasColour TextTag where
  legalColourID = hasForeGroundColour

-- | A text tag has a configureable font.
instance HasFont TextTag

-- | A text tag has a configureable text justification.
instance HasJustify TextTag

-- | A text tag has a configureable line spacing.
instance HasLineSpacing TextTag

-- | A text tag has adjustable tab stops.
instance HasTabulators TextTag

-- | You can synchronize on a text tag object.
instance Synchronized TextTag where
  -- Synchronizes on a text tag object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- Tag Commands
-- -----------------------------------------------------------------------

-- | Adds the specified text range to a text tag.
addTextTag :: (HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex) =>
   TextTag
   -- ^ the concerned text tag.
   -> i1
   -- ^ the start index.
   -> i2
   -- ^ the end index.
   -> IO ()
   -- ^ None.
addTextTag tag@(TextTag tp _) start end =
  synchronize tag (
    do
      start' <- getBaseIndex tp start
      end' <- getBaseIndex tp end
      execMethod tag (\nm -> tkTagAdd nm start' end')
  )

-- | Removes the specified text range from a text tag.
removeTextTag :: (HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex) =>
   TextTag
   -- ^ the concerned text tag.
   -> i1
   -- ^ the start index.
   -> i2
   -- ^ the end index.
   -> IO ()
   -- ^ None.
removeTextTag tag @ (TextTag tp _) start end =
  synchronize tag (
    do
      start' <- getBaseIndex tp start
      end' <- getBaseIndex tp end
      execMethod tag (\nm -> tkTagRemove nm start' end')
  )

-- | Lowers the text tag.
lowerTextTag :: TextTag
   -- ^ the concerned text tag.
   -> IO ()
   -- ^ None.
lowerTextTag tag = execMethod tag (\nm -> tkTagLower nm)

-- | Raises the given text tag.
raiseTextTag :: TextTag
   -- ^ the concerned text tag.
   -> IO ()
   -- ^ None.
raiseTextTag tag = execMethod tag (\nm -> tkTagRaise nm)


-- -----------------------------------------------------------------------
-- tag configure options
-- -----------------------------------------------------------------------

-- | Sets the normal left intend for a line.
lmargin1 :: Distance -> Config TextTag
lmargin1 s tag = cset tag "lmargin1" s

-- | Gets the normal left intend for a line.
getLmargin1 :: TextTag -> IO Distance
getLmargin1 tag = cget tag "lmargin1"

-- | Sets the intend for a part of a line that gets wrapped.
lmargin2 :: Distance -> Config TextTag
lmargin2 s tag = cset tag "lmargin2" s

-- | Gets the intend for a part of a line that gets wrapped.
getLmargin2 :: TextTag -> IO Distance
getLmargin2 tag = cget tag "lmargin2"

-- | Sets the right-hand margin.
rmargin :: Distance -> Config TextTag
rmargin s tag = cset tag "rmargin" s

-- | Gets the right-hand margin.
getRmargin :: TextTag -> IO Distance
getRmargin tag = cget tag "rmargin"

-- | Sets the baseline offset (positive for superscripts).
offset :: Distance -> Config TextTag
offset s tag = cset tag "offset" s

-- | Gets the baseline offset.
getOffset :: TextTag -> IO Distance
getOffset tag = cget tag "offset"

-- | If @True@, the text is drawn with a horizontal line through
-- it.
overstrike :: Toggle -> Config TextTag
overstrike s tag = cset tag "overstrike" s

-- | Gets the current overstrike setting.
getOverstrike :: TextTag -> IO Toggle
getOverstrike tag = cget tag "overstrike"

-- | If @True@, the text is underlined.
underlined :: Toggle -> Config TextTag
underlined s tag = cset tag "underline" s

-- | Gets the current underline setting.
getUnderlined :: TextTag -> IO Toggle
getUnderlined tag = cget tag "underline"

-- | Sets a stipple pattern for the background colour.
bgstipple :: BitMapHandle -> Config TextTag
bgstipple s tag = setBitMapHandle tag "bgstipple" s False

-- | Gets the stipple pattern for the background colour.
getBgstipple ::TextTag -> IO BitMapHandle
getBgstipple tag = getBitMapHandle tag "bgstipple"

-- | Sets a stipple pattern for the foreground colour.
fgstipple :: BitMapHandle -> Config TextTag
fgstipple s tag = setBitMapHandle tag "fgstipple" s False

-- | Gets the stipple pattern for the foreground colour.
getFgstipple :: TextTag -> IO BitMapHandle
getFgstipple tag = getBitMapHandle tag "fgstipple"


-- -----------------------------------------------------------------------
-- Index: Tag First and Last
-- -----------------------------------------------------------------------

-- | Internal.
instance HasIndex Editor (TextTag, First) BaseIndex where
  getBaseIndex tp (tag,_) =
    synchronize tag (
      do
        (pnm, tnm) <- getTagName tag
        return (IndexText (show tnm ++ ".first"))
    )

-- | Internal.
instance HasIndex Editor (TextTag, Last) BaseIndex where
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
               mkBoundCmdArg bindTag eventInfoSet False
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
