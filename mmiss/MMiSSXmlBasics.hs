-- | This function contains basic general XML functions we shall need.
module MMiSSXmlBasics(
   getAttribute,
   delAttribute,
   setAttribute,
   setAttribute0,
   getAtt,
   delAtt,
   setAtt,
   ) where

import Text.XML.HaXml.Types

import Computation
import ExtendedPrelude

-- ------------------------------------------------------------------------
-- Functions for manipulating [Attribute] values.  It is assumed that
-- all values are simple strings.
-- ------------------------------------------------------------------------

-- | Get an object's attribute. 
getAttribute :: [Attribute] -> String -> WithError (Maybe String)
getAttribute attributes name = case lookup name attributes of
   Just (AttValue [Left value]) -> return (Just value)
   Just _ -> fail ("Value of " ++ name ++ " attribute is not a simple string")
   Nothing -> return Nothing 

-- | Delete an attribute with a given key, if one is present
delAttribute :: [Attribute] -> String -> [Attribute]
delAttribute attributes0 key =
   deleteFirstOpt (\ (key1,_) -> key1 == key) attributes0

-- | Set an attribute
setAttribute :: [Attribute] -> String -> String -> [Attribute]
setAttribute attributes0 key value =
   let
      attributes1 = delAttribute attributes0 key
      attributes2 = setAttribute0 attributes1 key value
   in
      attributes2


-- | Only to be used when it is known that the attribute being added
-- does not occur in the input attributes.
setAttribute0 :: [Attribute] -> String -> String -> [Attribute]
setAttribute0 attributes0 key value =
   (key,AttValue [Left value]) : attributes0
   


-- | Get an Element's attribute
getAtt :: String -> Element -> WithError (Maybe String)
getAtt key (Elem _ atts _) = getAttribute atts key

-- | Delete an Element's attribute (or nothing, if it isn't set)
delAtt :: String -> Element -> Element
delAtt key (Elem name atts0 content) =
   let
      atts1 = delAttribute atts0 key
   in
      Elem name atts1 content
      
-- | Set an Element's attribute
setAtt :: String -> Element -> String -> Element
setAtt key (Elem name atts0 content) value =
   let
      atts1 = setAttribute atts0 key value
   in
      (Elem name atts1 content)

