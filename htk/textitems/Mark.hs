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

data Mark = Mark Editor String deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

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

setMarkGravity :: Mark -> Gravity -> IO ()
setMarkGravity mark @ (Mark tp name) grav =     
  execMethod tp (\nm -> tkSetMarkGravity nm name grav)
 where tkSetMarkGravity tnm mnm g =
         [show tnm ++ " mark gravity " ++ show mnm ++ " " ++ show g]

getMarkGravity :: Mark -> IO ()
getMarkGravity mark @ (Mark tp name) =  
  evalMethod tp (\nm -> tkGetMarkGravity nm name)
 where tkGetMarkGravity tnm mnm =
         [show tnm ++ " mark gravity " ++ show mnm]

unsetMark :: Mark -> IO ()
unsetMark (Mark tp name) = execMethod tp (\nm -> tkMarkUnset nm name)
 where tkMarkUnset nm mname  = [show nm ++ " mark unset " ++ show mname]

setMark :: HasIndex Editor i BaseIndex => Mark -> i -> IO ()
setMark mk@(Mark tp name) i =
  do
    binx <- getBaseIndex tp  i
    execMethod tp (\nm -> tkMarkSet nm name binx)

getCurrentMarks :: Editor -> IO [Mark]
getCurrentMarks tp =
  do
    str <- evalMethod tp (\nm -> [show nm ++ " mark names "])
    return (map (Mark tp) (words str))

-- -----------------------------------------------------------------------
-- Index
-- -----------------------------------------------------------------------

data MousePosition = MousePosition Editor

instance HasIndex Editor Mark BaseIndex where
  getBaseIndex w (Mark _ str) = return (IndexText str)

instance HasIndex Editor (Selection Editor) BaseIndex where
  getBaseIndex w p = return (IndexText "sel")

instance HasIndex Editor (ICursor Editor) BaseIndex where
  getBaseIndex w p = return (IndexText "insert")

instance HasIndex Editor MousePosition BaseIndex where
  getBaseIndex w p = return (IndexText "current")


-- -----------------------------------------------------------------------
-- Gravity
-- -----------------------------------------------------------------------

data Gravity = ToLeft | ToRight deriving (Eq,Ord,Enum)

instance Read Gravity where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(ToLeft,xs)]
        'r':'i':'g':'h':'t':xs -> [(ToRight,xs)]
        _ -> []

instance Show Gravity where
   showsPrec d p r = 
      (case p of 
         ToLeft -> "left"  
         ToRight -> "right"  
        ) ++ r

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
