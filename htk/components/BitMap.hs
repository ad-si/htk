{- #######################################################################

MODULE        : BitMap
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Bitmap Item

   #################################################################### -}


module BitMap (

  BitMap,
  newBitMap,

  BitMapHandle(..),
  HasBitMap(..),
  BitMapDesignator(..),

  errmap, 
  gray50, 
  gray25, 
  hourglass, 
  info, 
  questhead, 
  question, 
  warning,

  setBitMapHandle,
  getBitMapHandle,
  stringToBitMapHandle

) where

import GUIValue
import Core
import BaseClasses(Widget)
import Configuration
import Char(isDigit)
import qualified Posix (getEnvVar)
import Computation
import Synchronized
import Destructible
import Packer


-- -----------------------------------------------------------------------
-- BitMap designators
-- -----------------------------------------------------------------------

data BitMapHandle = 
          Predefined String 
        | BitMapHandle BitMap 
        | BitMapFile String

class BitMapDesignator d where
  toBitMap :: d -> BitMapHandle

instance BitMapDesignator BitMapHandle where
  toBitMap = id

instance BitMapDesignator BitMap where
  toBitMap h = BitMapHandle h

instance BitMapDesignator [Char] where
  toBitMap h = BitMapFile h


-- -----------------------------------------------------------------------
-- BitMap'ed widgets  
-- -----------------------------------------------------------------------

class GUIObject w => HasBitMap w where
  bitmap          :: BitMapDesignator d => d -> Config w
  getBitMap       :: w -> IO BitMapHandle
  bitmap d w      = setBitMapHandle w "bitmap" (toBitMap d) True 
  getBitMap w     = getBitMapHandle w "bitmap"


-- -----------------------------------------------------------------------
-- type BitMap 
-- -----------------------------------------------------------------------

newtype BitMap = BitMapWDG GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

newBitMap :: Container par => par -> [Config BitMap] -> IO BitMap
newBitMap par confs =
  do
    w <- createWidget (toGUIObject par) LABEL 
    configure (BitMapWDG w) confs


-- -----------------------------------------------------------------------
-- predefined Tk BitMaps
-- -----------------------------------------------------------------------

errmap, gray50, gray25, hourglass, info, questhead, question, warning :: BitMapHandle

errmap = Predefined "error" 
gray50 = Predefined "gray50" 
gray25 = Predefined "gray25" 
hourglass = Predefined "hourglass"
info = Predefined "info" 
questhead = Predefined "questhead"
question = Predefined "question" 
warning = Predefined "warning"


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

instance GUIObject BitMap where 
        toGUIObject (BitMapWDG w) = w
        cname _ = "BitMap"

instance Destroyable BitMap where
  destroy   = destroy . toGUIObject

instance Widget BitMap

instance HasBorder BitMap

instance HasColour BitMap where 
  legalColourID = hasForeGroundColour

instance HasSize BitMap

instance HasFile BitMap where
  filename fname w =
    execTclScript [tkBitMapCreate no fname] >> cset w "image" no
    where no = getObjectNo (toGUIObject w)
  getFileName w = evalTclScript [tkGetBitMapFile no]
    where no = getObjectNo (toGUIObject w)

instance Synchronized BitMap where
  synchronize (BitMapWDG w) = synchronize w


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

setBitMapHandle :: GUIObject w => w -> ConfigID -> BitMapHandle ->
                   Bool -> IO w
setBitMapHandle w cnm (Predefined d) _ = cset w cnm d 
setBitMapHandle w cnm (BitMapFile f) _ = cset w cnm ('@':f)
setBitMapHandle w _ (BitMapHandle h) True =
  cset w "image" (getObjectNo (toGUIObject h))
setBitMapHandle w cnm (BitMapHandle h) False =
  do
    fname <- getFileName h
    setBitMapHandle w cnm (BitMapFile fname) False
    return w
{- 
   the last parameter determines whether integer numbers are acceptable
   as bitmap denotations or not. If not, we use the corresponding file
   name associated with the widget! Numbers are allowed for labels and
   buttons, but not for windows!
-}

getBitMapHandle :: GUIObject w => w -> ConfigID -> IO BitMapHandle
getBitMapHandle w cnm = cget w cnm >>= stringToBitMapHandle

stringToBitMapHandle :: String -> IO BitMapHandle
stringToBitMapHandle "" = return (Predefined "")
stringToBitMapHandle ('@':tl) = return (BitMapFile tl)          
stringToBitMapHandle (str @ (x:tl)) | isDigit x = 
  lookupGUIObject (read str)      >>= return . BitMapHandle . BitMapWDG 
stringToBitMapHandle str = return (Predefined str)


-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkBitMapCreate :: Int -> String -> String
tkBitMapCreate no f = "image create bitmap " ++ show no ++ " -file " ++ show f
{-# INLINE tkBitMapCreate #-}

tkGetBitMapFile :: Int -> String
tkGetBitMapFile no = (show no) ++ " cget -file "
{-# INLINE tkGetBitMapFile #-}
