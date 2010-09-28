-- | This module contains the function for changing an object's attributes.
module MMiSS.EditAttributes(
   editObjectAttributes
   ) where

import Util.Computation(done)

import Types.Link
import Types.View

import MMiSS.ObjectType
import MMiSS.ObjectTypeInstance
import MMiSS.VariantObject

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
