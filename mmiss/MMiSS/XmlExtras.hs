-- | This module contains various functions Malcolm forgot to include
-- in HaXml, or which at least I haven't been able to find there.
module MMiSS.XmlExtras(
   fromDefaultable,
      -- :: Defaultable a -> a
   toDefaultable,
      -- :: Eq a => a -> a -> Defaultable a
      -- the first argument is the default.

   elementFoldM,
      -- :: Monad m
      -- => (ancestorInfo -> Element -> m ancestorInfo)
      -- -> ancestorInfo
      -- -> Element
      -- -> m ()

   getAllElementsByTag1, -- :: (String -> Bool) -> Element -> [Element]
   getAllElements, -- :: Element -> [Element]
   getAllElements1, -- :: Element -> [Element]
   mapElement, -- :: (Element -> Maybe Element) -> Element -> Element
   ) where

import Data.Maybe

import Control.Monad.State

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.Types

fromDefaultable :: Defaultable a -> a
fromDefaultable (Default a) = a
fromDefaultable (NonDefault a) = a

toDefaultable :: Eq a => a -> a -> Defaultable a
toDefaultable def a
   | def == a
   = Default def
toDefaultable _ a = NonDefault a


elementFoldM :: Monad m
   => (ancestorInfo -> Element -> m ancestorInfo)
   -> ancestorInfo
   -> Element
   -> m ()
elementFoldM visitElement ancestorInfo0 (element @ (Elem _ _ contents)) =
   do
      ancestorInfo1 <- visitElement ancestorInfo0 element
      let
         subElements =
            mapMaybe
               (\ content -> case content of
                  CElem elem -> Just elem
                  _ -> Nothing
                  )
               contents
      mapM_ (elementFoldM visitElement ancestorInfo1) subElements

-- | Return every constituent element of an element (including itself) in
-- order.
getAllElements :: Element -> [Element]
getAllElements element = reverse (getAllElements1 element)

-- | Return every constituent element of an element (including itself) in
-- reverse order.
getAllElements1 :: Element -> [Element]
getAllElements1 element = execState (elementFoldM foldFn () element) []
   where
      foldFn :: () -> Element -> State [Element] ()
      foldFn () element =
         do
            elements0 <- get
            put (element : elements0)

-- | Return all elements whose tags match a particular conditions
getAllElementsByTag1 :: (String -> Bool) -> Element -> [Element]
getAllElementsByTag1 tagFilter element =
   filter
      (\ (Elem tag _ _) -> tagFilter tag)
      (getAllElements1 element)


-- | Apply the given function to the Element, and if it returns Nothing
-- recurse.
mapElement :: (Element -> Maybe Element) -> Element -> Element
mapElement changeElement elem0 =
   case changeElement elem0 of
      Just elem1 -> elem1
      Nothing ->
         let
            Elem name atts contents0 = elem0
            contents1 = map
               (\ content -> case content of
                  CElem elem -> CElem (mapElement changeElement elem)
                  otherContent -> otherContent
                  )
               contents0
         in
            Elem name atts contents1
