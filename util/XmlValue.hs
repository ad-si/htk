{- This module contains the XmlValue class, which defines how
   values are represented as XML attributes. -}
module XmlValue(XmlValue(..)) where

class XmlValue value where
   ---
   -- The following functions should be  provided for parsing and unparsing
   -- the attributes to and from String values (for example, from XML files).
   parse :: String -> Maybe value
   unParse :: value -> String

   ---
   -- The following function is not used externally, but if defined
   -- to an appropriate version of readsPrec will provide an appropriate
   -- version of parse.
   rPrec :: Int -> String -> [(value,String)]

   parse s = case rPrec 0 s of
      [(value,"")] -> Just value
      _ -> Nothing

---
-- Where possible we accept this definition (which may of course
-- be overruled by one more specific).
instance (Read value,Show value) => XmlValue value where
   unParse = show
   rPrec = readsPrec
   

