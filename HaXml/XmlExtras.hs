{- This module contains various functions Malcolm forgot to include
   in HaXml. -}
module XmlExtras(
   fromDefaultable,
      -- :: Defaultable a -> a
   toDefaultable,
      -- :: Eq a => a -> a -> Defaultable a
      -- the first argument is the default.
   ) where

import Text.XML.HaXml.Xml2Haskell

fromDefaultable :: Defaultable a -> a
fromDefaultable (Default a) = a
fromDefaultable (NonDefault a) = a

toDefaultable :: Eq a => a -> a -> Defaultable a
toDefaultable def a 
   | def == a
   = Default def
toDefaultable _ a = NonDefault a


 

