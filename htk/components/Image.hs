{- #########################################################################

MODULE        : Image
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Image's in the sense of Tk


   ######################################################################### -}


module Image (
        HasPhoto(..),
        Image,  
        newImage,

        intToImage,
        imageToInt,

	Format(..),
	imgData

        ) where

import Concurrency
import GUICore
import Packer
import Debug(debug)

-- --------------------------------------------------------------------------
-- Class Image 
-- --------------------------------------------------------------------------            
class GUIObject w => HasPhoto w where
        photo           :: Image -> Config w
        getPhoto        :: w -> IO (Maybe Image)
        photo img w     = imageToInt img >>= cset w "image"
        getPhoto w      = cget w "image" >>= intToImage 


-- --------------------------------------------------------------------------
-- Type Image 
-- --------------------------------------------------------------------------           
newtype Image = Image GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor 
-- --------------------------------------------------------------------------           
newImage :: [Config Image] -> IO Image
newImage ol = do
        w <- createWidget LABEL
        configure (Image w) ol

imgData :: Format -> String  -> Config Image
imgData f str w =
  synchronize w (do {
	execTclScript [tkImageCreateFromData no f str] >>
        cset w "image" no})
  where no = getObjectNo (toGUIObject w)

data Format = GIF | PPM | PGM

formatToString :: Format -> String
formatToString f =
  case f of
    GIF -> "GIF"
    PPM -> "PPM"
    _   -> "PGM"


-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance GUIObject Image where 
        toGUIObject (Image w) = w; cname _ = "Image"

instance Destructible Image where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Widget Image
 
instance ChildWidget Image 

instance HasBorder Image

instance HasColour Image where 
        legalColourID = hasForeGroundColour

instance HasSize Image

instance HasFile Image where
        filename str w = 
                synchronize w (do {                     
                        execTclScript [tkImageCreate no str] >>
                        cset w "image" no
                        }) 
                where no = getObjectNo (toGUIObject w) 
        getFileName w = evalTclScript [tkGetImageFile no] 
                where no = getObjectNo (toGUIObject w)

instance Synchronized Image where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
-- Auxiliary Functions 
-- --------------------------------------------------------------------------           
intToImage :: Int -> IO (Maybe Image)
intToImage 0 = return Nothing
intToImage no = lookupGUIObject (ObjectID no) >>= return . Just . Image 

{- this function converts the Tk representation of an image to the HTK
   representation. Needed by several other image retrieval function.
-}

imageToInt :: Image -> IO Int
imageToInt = return . getObjectNo . toGUIObject

-- --------------------------------------------------------------------------
-- Tk Commands 
-- --------------------------------------------------------------------------           
tkImageCreate :: Int -> String -> String
tkImageCreate no file = "image create photo " ++ show no ++ " -file " ++ show file
{-# INLINE tkImageCreate #-}

tkGetImageFile :: Int -> String
tkGetImageFile no = (show no) ++ " cget -file "
{-# INLINE tkGetImageFile #-}

tkImageCreateFromData :: Int -> Format -> String -> String
tkImageCreateFromData no f dat = "image create photo " ++ show no ++ " -data " ++ show dat ++ " -format " ++ show (formatToString f)