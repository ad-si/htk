{- #########################################################################

MODULE        : Mark
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Mark items for Text Widget


   ######################################################################### -}


module Mark (
        Editor,

        Gravity(..),

        Mark(..),
        newMark,
        setMarkGravity,

        setMark,
        unsetMark,

        getCurrentMarks


        ) where

import Concurrency
import GUICore

import Index
import Selection
import Editor
import Char(isSpace)
import Debug(debug)


-- --------------------------------------------------------------------------
-- Marks
-- --------------------------------------------------------------------------

data Mark a = Mark (Editor a) String deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newMark :: HasIndex (Editor a) i BaseIndex => 
                (Editor a) -> String -> i -> IO (Mark a)
newMark tp name i = 
        synchronize tp (do {
                ix <- getBaseIndex tp i;
                execMethod tp (\nm -> tkMarkSet nm name ix);
                return (Mark tp name)
                })


-- --------------------------------------------------------------------------
-- Mark Operations
-- --------------------------------------------------------------------------

setMarkGravity :: Mark a -> Gravity -> IO ()
setMarkGravity mark @ (Mark tp name) grav =     
        execMethod tp (\nm -> tkSetMarkGravity nm name grav)
 where  tkSetMarkGravity tnm mnm g =
                [show tnm ++ " mark gravity " ++ show mnm ++ " " ++ show g]


getMarkGravity :: Mark a -> IO ()
getMarkGravity mark @ (Mark tp name) =  
        evalMethod tp (\nm -> tkGetMarkGravity nm name)
 where  tkGetMarkGravity tnm mnm = [show tnm ++ " mark gravity " ++ show mnm]


unsetMark :: Mark a -> IO ()
unsetMark (Mark tp name) =  
        execMethod tp (\nm -> tkMarkUnset nm name)
 where  tkMarkUnset nm mname  = [show nm ++ " mark unset " ++ show mname]


setMark :: HasIndex (Editor a) i BaseIndex => Mark a -> i -> IO ()
setMark mk@(Mark tp name) i =  do
        binx <- getBaseIndex tp  i
        execMethod tp (\nm -> tkMarkSet nm name binx)


getCurrentMarks :: Editor a -> IO [Mark a]
getCurrentMarks tp = do
        str <- evalMethod tp (\nm -> [show nm ++ " mark names "])
        return (map (Mark tp) (words str))
        

-- --------------------------------------------------------------------------
-- Index
-- --------------------------------------------------------------------------

data MousePosition a = MousePosition (Editor a)

instance HasIndex (Editor a) (Mark a) BaseIndex where
        getBaseIndex w (Mark _ str) = return (IndexText str)


instance HasIndex (Editor a) (Selection (Editor a)) BaseIndex where
        getBaseIndex w p = return (IndexText "sel")


instance HasIndex (Editor a) (ICursor (Editor a)) BaseIndex where
        getBaseIndex w p = return (IndexText "insert")


instance HasIndex (Editor a) (MousePosition (Editor a)) BaseIndex where
        getBaseIndex w p = return (IndexText "current")


-- --------------------------------------------------------------------------
-- Gravity
-- --------------------------------------------------------------------------

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


-- --------------------------------------------------------------------------
-- Unparsing of Mark Commands 
-- --------------------------------------------------------------------------

tkMarkSet :: ObjectName -> String -> BaseIndex -> TclScript
tkMarkSet tname mname ix =
        [show tname ++ " mark set " ++ show mname ++ " " ++ ishow ix]

ishow :: BaseIndex -> String
ishow i = "{" ++ show i ++ "}"
