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
-- This module provides access to text marks inside an editor widget.
module Mark (

  Editor,

  Gravity(..),

  Mark(..),
  createMark,
  setMarkGravity,

  setMark,
  unsetMark,

  getCurrentMarks

) where

import Core
import Index
import Selection
import Editor
import Char(isSpace)
import Synchronized


-- -----------------------------------------------------------------------
-- type Mark
-- -----------------------------------------------------------------------

---
-- The <code>Mark</code> datatype.
data Mark = Mark Editor String deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Creates a text mark inside an editor widget and returns a handler.
-- @param ed      - the concerned editor widget.
-- @param name    - the name of the text mark to create.
-- @param i       - the text marks index position inside the editor
--                - widget.
-- @return result - A text mark.
createMark :: HasIndex Editor i BaseIndex => 
              Editor -> String -> i -> IO Mark
createMark ed name i =
  synchronize ed (do
                    ix <- getBaseIndex ed i
                    execMethod ed (\nm -> tkMarkSet nm name ix)
                    return (Mark ed name))


-- -----------------------------------------------------------------------
-- Mark Operations
-- -----------------------------------------------------------------------

---
-- Sets the gravity of the given text mark.
-- @param mark    - the concerned text mark.
-- @param grav    - the gravity to set.
-- @return result - None.
setMarkGravity :: Mark -> Gravity -> IO ()
setMarkGravity mark @ (Mark tp name) grav =     
  execMethod tp (\nm -> tkSetMarkGravity nm name grav)
 where tkSetMarkGravity tnm mnm g =
         [show tnm ++ " mark gravity " ++ show mnm ++ " " ++ show g]

---
-- Gets the gravity from the given text mark.
-- @param mark    - the concerned text mark.
-- @return result - The current gravity setting.
getMarkGravity :: Mark -> IO Gravity
getMarkGravity mark @ (Mark tp name) =  
  evalMethod tp (\nm -> tkGetMarkGravity nm name)
 where tkGetMarkGravity tnm mnm =
         [show tnm ++ " mark gravity " ++ show mnm]

---
-- Unsets a text mark inside an editor widget.
-- @param mark    - the concerned text mark.
-- @return result - None.
unsetMark :: Mark -> IO ()
unsetMark mark@(Mark tp name) = execMethod tp (\nm -> tkMarkUnset nm name)
 where tkMarkUnset nm mname  = [show nm ++ " mark unset " ++ show mname]

---
-- Sets the index position of the text mark.
-- @param mark    - the concerned tex mark.
-- @return result - None.
setMark :: HasIndex Editor i BaseIndex => Mark -> i -> IO ()
setMark mark@(Mark tp name) i =
  do
    binx <- getBaseIndex tp  i
    execMethod tp (\nm -> tkMarkSet nm name binx)

---
-- Gets the current marks from an editor widget.
-- @param ed      - the concerned editor widget.
-- @return result - A list of text marks.
getCurrentMarks :: Editor -> IO [Mark]
getCurrentMarks ed =
  do
    str <- evalMethod ed (\nm -> [show nm ++ " mark names "])
    return (map (Mark ed) (words str))


-- -----------------------------------------------------------------------
-- Index
-- -----------------------------------------------------------------------

---
-- The <code>MousePosition</code> datatype.
data MousePosition = MousePosition Editor

---
-- Internal.
instance HasIndex Editor Mark BaseIndex where
---
-- Internal.
  getBaseIndex w (Mark _ str) = return (IndexText str)

---
-- Internal.
instance HasIndex Editor (Selection Editor) BaseIndex where
---
-- Internal.
  getBaseIndex w p = return (IndexText "sel")

---
-- Internal.
instance HasIndex Editor (ICursor Editor) BaseIndex where
---
-- Internal.
  getBaseIndex w p = return (IndexText "insert")

---
-- Internal.
instance HasIndex Editor MousePosition BaseIndex where
---
-- Internal.
  getBaseIndex w p = return (IndexText "current")


-- -----------------------------------------------------------------------
-- Gravity
-- -----------------------------------------------------------------------

---
-- The <code>Gravity</code> datatype.
data Gravity = ToLeft | ToRight deriving (Eq,Ord,Enum)

---
-- Internal.
instance Read Gravity where
---
-- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(ToLeft,xs)]
        'r':'i':'g':'h':'t':xs -> [(ToRight,xs)]
        _ -> []

---
-- Internal.
instance Show Gravity where
---
-- Internal.
   showsPrec d p r = 
      (case p of 
         ToLeft -> "left"  
         ToRight -> "right"  
        ) ++ r

---
-- Internal.
instance GUIValue Gravity where
---
-- Internal.
  cdefault = ToLeft


-- -----------------------------------------------------------------------
-- unparsing of Mark commands
-- -----------------------------------------------------------------------

tkMarkSet :: ObjectName -> String -> BaseIndex -> TclScript
tkMarkSet tname mname ix =
  [show tname ++ " mark set " ++ show mname ++ " " ++ ishow ix]

ishow :: BaseIndex -> String
ishow i = "{" ++ show i ++ "}"
