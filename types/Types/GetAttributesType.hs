-- | Contains code enabling the user to define an AttributesType
-- NB.  At the moment this is disabled, and will always return the
-- empty attributes.  It should not be renabled again unless and until
-- bundles can handle attributes.
module Types.GetAttributesType(
   getAttributesType, -- :: IO AttributesType
   ) where

import Types.AttributesType

getAttributesType :: IO (Maybe AttributesType)
getAttributesType = return (Just emptyAttributesType)

