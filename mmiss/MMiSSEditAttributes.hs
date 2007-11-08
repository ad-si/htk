-- | This module contains the function for changing an object's attributes.
module MMiSSEditAttributes(
   editObjectAttributes
   ) where

import Computation(done)

import Link
import View

import MMiSSObjectType
import MMiSSObjectTypeInstance
import MMiSSVariantObject

editObjectAttributes :: View -> Link MMiSSObject -> IO ()
editObjectAttributes view link =
   do
      object <- readLink view link
      name <- objectName object
      changed <- editMMiSSSearchObject name (variantObject object)
      if changed
         then
            dirtyLink view link
         else
            done
