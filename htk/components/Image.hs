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

#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif


---
-- This module provides access to image resources from files or base64
-- encoded strings.
module Image (

  HasPhoto(..),
  Image,  
  newImage,

  intToImage,
  imageToInt,

  Format(..),
  imgData,
  imgData#,

) where

#ifdef NEW_GHC
import GHC.Ptr(Ptr(Ptr))
#else
import PrelPtr(Ptr(Ptr))
#endif
import GlaExts(Addr#)
import CString

import Core
import BaseClasses(Widget)
import Configuration
import Computation
import Synchronized
import Destructible
import Packer


-- -----------------------------------------------------------------------
-- class image
-- -----------------------------------------------------------------------

---
-- Image containers instantiate the <code>class HasPhoto</code>.
class GUIObject w => HasPhoto w where
---
-- Associates an image container (e.g. a label) with the given image.
  photo           :: Image -> Config w
---
-- Gets the image associated with the given image container.
  getPhoto        :: w -> IO (Maybe Image)
  photo img w     = imageToInt img >>= cset w "image"
  getPhoto w      = cget w "image" >>= intToImage 


-- -----------------------------------------------------------------------
-- type image
-- -----------------------------------------------------------------------

---
-- The <code>Image</code> datatype.
newtype Image = Image GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new image object and returns a handler.<br>
-- The image object can be packed like a widget, then it is implicitely
-- displayed inside a label widget.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this image
--                  object.
-- @return result - An image object.
newImage :: Container par => par -> [Config Image] -> IO Image
newImage par cnf =
  do
    w <- createWidget (toGUIObject par) LABEL
    configure (Image w) cnf

---
-- Sets the image data from a base64 encoded string.
imgData :: Format -> String  -> Config Image
imgData f str w =
    execTclScript [tkImageCreateFromData no f str] >> cset w "image" no
  where no = getObjectNo (toGUIObject w)

---
-- Like imgData, but takes an Addr#.  
--
-- An Addr# can be written like a string constant, except you add an extra
-- "#" character after the closing quotation.
-- The Addr# MUST be null terminated (or chaos could result).  So
-- instead of writing 
-- "ThisIsAnImageEncodedInBase64Notation" you must write
-- "ThisIsAnImageEncodedInBase64Notation\0"#
-- The advantage of this is that the string is written directly into the
-- object file, without GHC's optimiser bringing the compilation to a halt 
-- while it spends ages optimising constructing the string.
imgData# :: Format -> Addr#  -> Config Image
imgData# f addr w =
   do
      str <- peekCString (Ptr addr)
      imgData f str w

---
-- The <code>Format</code> datatype - represents the format of a base64
-- encoded image (see <code>Image.imgData</code>).
data Format = GIF | PPM | PGM

formatToString :: Format -> String
formatToString f =
  case f of
    GIF -> "GIF"
    PPM -> "PPM"
    _   -> "PGM"


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Image where 
---
-- Internal.
  toGUIObject (Image w) = w
---
-- Internal.
  cname _ = "Image"

---
-- An image object can be destroyed.
instance Destroyable Image where
---
-- Destroys an image object.
  destroy = destroy . toGUIObject

---
-- An image object has standard widget properties
-- (concerning focus, cursor / if implicitely displayed inside a label
-- widget).
instance Widget Image

---
-- An image object has a configureable border (if implicitely displayed
-- inside a label widget).
instance HasBorder Image

---
-- An image object has a configureable foreground and background colour
-- (if implicitely displayed inside a label widget).
instance HasColour Image where
  legalColourID = hasForeGroundColour

---
-- You can specify the size of the containing label, if the image is
-- implicitely displayed inside a label widget.
instance HasSize Image

---
-- Images can be read from files.
instance HasFile Image where
---
-- Specifies the image file path.
  filename str w =
    execTclScript [tkImageCreate no str] >> cset w "image" no
    where no = getObjectNo (toGUIObject w) 
---
-- Gets the image's file name.
  getFileName w = evalTclScript [tkGetImageFile no] 
    where no = getObjectNo (toGUIObject w)

---
-- You can synchronize on an image object.
instance Synchronized Image where
---
-- Synchronizes on an image object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

---
-- Internal.
intToImage :: Int -> IO (Maybe Image)
intToImage 0 = return Nothing
intToImage no = lookupGUIObject (ObjectID no) >>= return . Just . Image 

{- this function converts the Tk representation of an image to the HTK
   representation. Needed by several other image retrieval function.
-}

---
-- Internal.
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
