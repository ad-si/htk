{-# LANGUAGE FlexibleInstances #-}

-- | This module provides access to image resources from files or base64
-- encoded strings.
module HTk.Components.Image (

  HasPhoto(..),
  Image,
  newImage,

  intToImage,
  imageToInt,

  Format(..),
  imgData,
  imgGamma,
  imgPalette
) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import Util.Computation
import Events.Synchronized
import Events.Destructible

-- -----------------------------------------------------------------------
-- class image
-- -----------------------------------------------------------------------

-- | Image containers instantiate the @class HasPhoto@.
class GUIObject w => HasPhoto w where
  -- Associates an image container (e.g. a label) with the given image.
  photo           :: Image -> Config w
  -- Gets the image associated with the given image container.
  getPhoto        :: w -> IO (Maybe Image)
  photo img w     = imageToInt img >>= cset w "image"
  getPhoto w      = cget w "image" >>= intToImage


-- -----------------------------------------------------------------------
-- type image
-- -----------------------------------------------------------------------

-- | The @Image@ datatype.
newtype Image = Image GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new image object and returns a handler.
-- The image object can be packed like a widget, then it is implicitely
-- displayed inside a label widget.
newImage :: [Config Image]
   -- ^ the list of configuration options for this image
   -- object.
   -> IO Image
   -- ^ An image object.
newImage cnf =
  do
    w <- createWidget ROOT LABEL
    configure (Image w) cnf

-- | Sets the image data from a base64 encoded string.
imgData :: Format -> String  -> Config Image
imgData f str w =
    execTclScript [tkImageCreateFromData no f str] >> cset w "image" no
  where no = getObjectNo (toGUIObject w)

-- | The @Format@ datatype - represents the format of a base64
-- encoded image (see @Image.imgData@).
data Format = GIF | PPM | PGM

formatToString :: Format -> String
formatToString f =
  case f of
    GIF -> "GIF"
    PPM -> "PPM"
    _   -> "PGM"


-- | The @gamma@ correction factor. Values less than one
-- darken the image, values greater than one brighten up the image.
imgGamma :: Double -> Config Image
imgGamma g = tkImgConfig ("-gamma "++ show g)

-- | The colour palette specifies a private palette for this image.
-- You can either specify a grayscale palette (of n shades of grey), or an
-- RGB triple.
class PaletteSpec p where
  -- Internal function only
  tkShowPalette :: p-> String

instance PaletteSpec Int where
  tkShowPalette p = show p

instance PaletteSpec (Int, Int, Int) where
  tkShowPalette (r, g, b) = show r ++ "/"++ show g++ "/"++ show b

imgPalette :: PaletteSpec p=> p-> Config Image
imgPalette p = tkImgConfig ("-palette "++ tkShowPalette p)


-- We leave the  getImgGamma and getImgPalette functions as exercises
-- to the interested reader of this source code.





-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Image where
  toGUIObject (Image w) = w
  cname _ = "Image"

-- | An image object can be destroyed.
instance Destroyable Image where
  -- Destroys an image object.
  destroy = destroy . toGUIObject

-- | An image object has standard widget properties
-- (concerning focus, cursor \/ if implicitely displayed inside a label
-- widget).
instance Widget Image

-- | An image object has a configureable border (if implicitely displayed
-- inside a label widget).
instance HasBorder Image

-- | An image object has a configureable foreground and background colour
-- (if implicitely displayed inside a label widget).
instance HasColour Image where
  legalColourID = hasForeGroundColour

-- | You can specify the size of the containing label, if the image is
-- implicitely displayed inside a label widget.
instance HasSize Image

-- | Images can be read from files.
instance HasFile Image where
  -- Specifies the image file path.
  filename str w =
    execTclScript [tkImageCreate no str] >> cset w "image" no
    where no = getObjectNo (toGUIObject w)
  -- Gets the image\'s file name.
  getFileName w = evalTclScript [tkGetImageFile no]
    where no = getObjectNo (toGUIObject w)

-- | You can synchronize on an image object.
instance Synchronized Image where
  -- Synchronizes on an image object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

-- | Internal.
intToImage :: Int -> IO (Maybe Image)
intToImage 0 = return Nothing
intToImage no = lookupGUIObject (ObjectID no) >>= return . Just . Image

{- this function converts the Tk representation of an image to the HTK
   representation. Needed by several other image retrieval function.
-}

-- | Internal.
imageToInt :: Image -> IO Int
imageToInt = return . getObjectNo . toGUIObject


-- -----------------------------------------------------------------------
-- Tk Commands
-- -----------------------------------------------------------------------

tkImageCreate :: Int -> String -> String
tkImageCreate no file = "image create photo " ++ show no ++ " -file " ++ show file
{-# INLINE tkImageCreate #-}

tkGetImageFile :: Int -> String
tkGetImageFile no = (show no) ++ " cget -file "
{-# INLINE tkGetImageFile #-}

tkImageCreateFromData :: Int -> Format -> String -> String
tkImageCreateFromData no f dat = "image create photo " ++ show no ++ " -data " ++ show dat ++ " -format " ++ show (formatToString f)

tkImgConfig :: String-> Config Image
tkImgConfig cstr w =
  do execTclScript [show no++ " configure "++ cstr]
     return w
  where no = getObjectNo (toGUIObject w)
{-# INLINE tkImgConfig #-}
