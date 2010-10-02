{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides access to text marks inside an editor widget.
module HTk.Textitems.Mark (

  Gravity(..),

  Mark(..),
  createMark,
  setMarkGravity,

  setMark,
  unsetMark,

  getCurrentMarks

) where

import HTk.Kernel.Core
import HTk.Components.Index
import HTk.Components.ICursor
import HTk.Components.Selection
import HTk.Widgets.Editor
import Data.Char(isSpace)
import Events.Synchronized


-- -----------------------------------------------------------------------
-- type Mark
-- -----------------------------------------------------------------------

-- | The @Mark@ datatype.
data Mark = Mark Editor String deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Creates a text mark inside an editor widget and returns a handler.
createMark :: HasIndex Editor i BaseIndex =>
   Editor
   -- ^ the concerned editor widget.
   -> String
   -- ^ the name of the text mark to create.
   -> i
   -- ^ the text marks index position inside the editor
   -- widget.
   -> IO Mark
   -- ^ A text mark.
createMark ed name i =
  synchronize ed (do
                    ix <- getBaseIndex ed i
                    execMethod ed (\nm -> tkMarkSet nm name ix)
                    return (Mark ed name))


-- -----------------------------------------------------------------------
-- Mark Operations
-- -----------------------------------------------------------------------

-- | Sets the gravity of the given text mark.
setMarkGravity :: Mark
   -- ^ the concerned text mark.
   -> Gravity
   -- ^ the gravity to set.
   -> IO ()
   -- ^ None.
setMarkGravity mark @ (Mark tp name) grav =
  execMethod tp (\nm -> tkSetMarkGravity nm name grav)
 where tkSetMarkGravity tnm mnm g =
         [show tnm ++ " mark gravity " ++ show mnm ++ " " ++ show g]

-- | Gets the gravity from the given text mark.
getMarkGravity :: Mark
   -- ^ the concerned text mark.
   -> IO Gravity
   -- ^ The current gravity setting.
getMarkGravity mark @ (Mark tp name) =
  evalMethod tp (\nm -> tkGetMarkGravity nm name)
 where tkGetMarkGravity tnm mnm =
         [show tnm ++ " mark gravity " ++ show mnm]

-- | Unsets a text mark inside an editor widget.
unsetMark :: Mark
   -- ^ the concerned text mark.
   -> IO ()
   -- ^ None.
unsetMark mark@(Mark tp name) = execMethod tp (\nm -> tkMarkUnset nm name)
 where tkMarkUnset nm mname  = [show nm ++ " mark unset " ++ show mname]

-- | Sets the index position of the text mark.
setMark :: HasIndex Editor i BaseIndex => Mark
   -- ^ the concerned tex mark.
   -> i
   -> IO ()
   -- ^ None.
setMark mark@(Mark tp name) i =
  do
    binx <- getBaseIndex tp  i
    execMethod tp (\nm -> tkMarkSet nm name binx)

-- | Gets the current marks from an editor widget.
getCurrentMarks :: Editor
   -- ^ the concerned editor widget.
   -> IO [Mark]
   -- ^ A list of text marks.
getCurrentMarks ed =
  do
    str <- evalMethod ed (\nm -> [show nm ++ " mark names "])
    return (map (Mark ed) (words str))


-- -----------------------------------------------------------------------
-- Index
-- -----------------------------------------------------------------------

-- | The @MousePosition@ datatype.
data MousePosition = MousePosition Editor

-- | Internal.
instance HasIndex Editor Mark BaseIndex where
  getBaseIndex w (Mark _ str) = return (IndexText str)

-- | Internal.
instance HasIndex Editor (Selection Editor) BaseIndex where
  getBaseIndex w p = return (IndexText "sel")

-- | Internal.
instance HasIndex Editor (ICursor Editor) BaseIndex where
  getBaseIndex w p = return (IndexText "insert")

-- | Internal.
instance HasIndex Editor MousePosition BaseIndex where
  getBaseIndex w p = return (IndexText "current")


-- -----------------------------------------------------------------------
-- Gravity
-- -----------------------------------------------------------------------

-- | The @Gravity@ datatype.
data Gravity = ToLeft | ToRight deriving (Eq,Ord,Enum)

-- | Internal.
instance Read Gravity where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(ToLeft,xs)]
        'r':'i':'g':'h':'t':xs -> [(ToRight,xs)]
        _ -> []

-- | Internal.
instance Show Gravity where
   showsPrec d p r =
      (case p of
         ToLeft -> "left"
         ToRight -> "right"
        ) ++ r

-- | Internal.
instance GUIValue Gravity where
  cdefault = ToLeft


-- -----------------------------------------------------------------------
-- unparsing of Mark commands
-- -----------------------------------------------------------------------

tkMarkSet :: ObjectName -> String -> BaseIndex -> TclScript
tkMarkSet tname mname ix =
  [show tname ++ " mark set " ++ show mname ++ " " ++ ishow ix]

ishow :: BaseIndex -> String
ishow i = "{" ++ show i ++ "}"
