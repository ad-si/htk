{- This module implements a simple list box to which strings can be
   added at the end and deleted. -}
module SimpleListBox(
   SimpleListBox,
   newSimpleListBox, 
      -- :: String -> (value -> String) -> (Distance,Distance) 
      -- -> IO (SimpleListBox value)
      -- Create a ListBox.  The String gives the title for the box; the
      -- function argument gives the String's which
      -- are displayed; the integers give the width (characters) and height 
      -- (rows) of the displayed section of the box.

      -- This implements Destroyable.

   SimpleListBoxItem,
      -- Instance of Object, Eq, Ord.
   addItemAtEnd,
      -- :: SimpleListBox value -> value -> IO (SimpleListBoxItem value)
   deleteItem,
      -- :: SimpleListBox value -> SimpleListBoxItem value -> IO ()

   bindSelection,
      -- :: SimpleListBox value 
      --   -> IO (Event (Maybe (SimpleListBoxItem value)),IO ())
      -- Returns an event for clicks in this list box.  (Nothing
      -- indicates that no list box was present here.)
   ) where

import Control.Concurrent.MVar

import ExtendedPrelude
import Object

import Destructible

import Events

import GUIObject(toGUIObject)
import HTk


-- -------------------------------------------------------------------------
-- Datatypes
-- -------------------------------------------------------------------------

data SimpleListBox val = SimpleListBox {
   topLevel :: Toplevel,
   listBox :: ListBox String,
   mkString :: val -> String,
   contentsMVar :: MVar [SimpleListBoxItem val]
   }

data SimpleListBoxItem val = SimpleListBoxItem {
   val :: val,
   oID :: ObjectID
   }

-- -------------------------------------------------------------------------
-- Instances
-- -------------------------------------------------------------------------

instance Object (SimpleListBox val) where
   objectID simpleListBox = objectID (toGUIObject (listBox simpleListBox))

instance Destroyable (SimpleListBox val) where
   destroy simpleListBox = destroy (topLevel simpleListBox)

instance Object (SimpleListBoxItem val) where
   objectID simpleListBoxItem = oID simpleListBoxItem

instance Eq (SimpleListBoxItem val) where
   (==) = mapEq oID

instance Ord (SimpleListBoxItem val) where
   compare = mapOrd oID

-- -------------------------------------------------------------------------
-- Functions
-- -------------------------------------------------------------------------

newSimpleListBox
   :: String -> (val -> String) -> (Distance,Distance)
   -> IO (SimpleListBox val)
newSimpleListBox title mkString (width,height) =
   do
      topLevel <- createToplevel [text title]

      listBox <- newListBox topLevel [value ([] :: [String]),
         bg "white",size (width,height)]
      pack listBox [Side AtLeft]

      scroll <- newScrollBar topLevel []
      pack scroll [Side AtRight,Fill Y]

      listBox # scrollbar Vertical scroll
      listBox # selectMode Extended

      contentsMVar <- newMVar []

      let 
         simpleListBox = SimpleListBox {
            topLevel = topLevel,
            listBox = listBox,
            mkString = mkString,
            contentsMVar = contentsMVar
            }

      return simpleListBox

addItemAtEnd :: SimpleListBox val -> val -> IO (SimpleListBoxItem val)
addItemAtEnd simpleListBox (val1 :: val) =
   do
      -- We have to recompute the complete list of Strings, since
      -- Einar doesn't seem to have provided a function for adding a single
      -- item to a ListBox, and I can't be bothered to implement one.
      let
         mVar = contentsMVar simpleListBox
         mkS = mkString simpleListBox

      oID <- newObject
      let
         simpleListBoxItem = SimpleListBoxItem {
            val = val1,
            oID = oID
            }

      contents0 <- takeMVar mVar
      let
         contents1 :: [SimpleListBoxItem val]
         contents1 = contents0 ++ [simpleListBoxItem]

         newValue :: [String]
         newValue = map (mkS . val) contents1

      (listBox simpleListBox) # value newValue
      putMVar mVar contents1

      return simpleListBoxItem

deleteItem :: SimpleListBox val -> SimpleListBoxItem val -> IO ()
deleteItem simpleListBox simpleListBoxItem =
   do
      let
         mVar = contentsMVar simpleListBox
         mkS = mkString simpleListBox

      contents0 <- takeMVar mVar
      let
         contents1 = deleteFirst (== simpleListBoxItem) contents0

         (newValue :: [String]) = map (mkS . val) contents1

      (listBox simpleListBox) # value newValue
      putMVar mVar contents1
      done
      
bindSelection :: SimpleListBox val 
   -> IO (Event (Maybe (SimpleListBoxItem val)),IO ())
bindSelection simpleListBox =
   do   
      (press,terminator) 
         <- bindSimple (listBox simpleListBox) (ButtonPress (Just 1))
      let
         event = 
               press 
            >>> 
               do
                  indexOpt <- getSelection (listBox simpleListBox)
                  contents0 <- readMVar (contentsMVar simpleListBox)
                  return (case indexOpt of
                     Nothing -> Nothing
                     Just [] -> Nothing
                     Just (index : _) -> 
                        if index >= length contents0 
                           then
                              -- possible at least if element got prematurely
                              -- deleted
                              Nothing
                           else
                              Just (contents0 !! index)
                     )
      return (event,terminator)