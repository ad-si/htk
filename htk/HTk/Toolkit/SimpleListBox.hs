{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a simple list box to which strings can be
-- added at the end and deleted.
module HTk.Toolkit.SimpleListBox(
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
   getItems,
      -- :: SimpleListBox value -> IO [value]

   bindSelection,
      -- :: SimpleListBox value
      --   -> IO (Event [SimpleListBoxItem value]),IO ())
      -- Returns an event for selections in this list box.  ([] can happen,
      -- for example, if the user selects or clicks an area into which no
      -- item has yet been added.)
   ) where

import Data.Maybe

import Control.Concurrent.MVar

import Util.ExtendedPrelude
import Util.Object
import Util.Computation

import Events.Events

import HTk.Kernel.Core(GUIObject(..))
import HTk.Toplevel.HTk


-- -------------------------------------------------------------------------
-- Datatypes
-- -------------------------------------------------------------------------

data SimpleListBox val = SimpleListBox {
   frame :: Frame, -- contains the list box, and the scroll-bar.
   listBox :: ListBox String,
   mkString :: val -> String,
   contentsMVar :: MVar [SimpleListBoxItem val]
   }

data SimpleListBoxItem val = SimpleListBoxItem {
   val :: val,
   oID :: ObjectID
   }

-- -------------------------------------------------------------------------
-- Non-HTk Instances
-- -------------------------------------------------------------------------

instance Object (SimpleListBox val) where
   objectID simpleListBox = objectID (toGUIObject simpleListBox)

instance Destroyable (SimpleListBox val) where
   destroy simpleListBox = destroy (frame simpleListBox)

instance Object (SimpleListBoxItem val) where
   objectID simpleListBoxItem = oID simpleListBoxItem

instance Eq (SimpleListBoxItem val) where
   (==) = mapEq oID

instance Ord (SimpleListBoxItem val) where
   compare = mapOrd oID

-- -------------------------------------------------------------------------
-- HTk instances (needed for packing a list box)
-- -------------------------------------------------------------------------

instance GUIObject (SimpleListBox val) where
   toGUIObject simpleListBox = toGUIObject (frame simpleListBox)

   cname _ = "SimpleListBox"

instance Widget (SimpleListBox val)

instance HasSize (SimpleListBox val)


-- -------------------------------------------------------------------------
-- Functions
-- -------------------------------------------------------------------------

newSimpleListBox
   :: Container par
   => par -> (val -> String) -> [Config (SimpleListBox val)]
   -> IO (SimpleListBox val)
newSimpleListBox parent mkString configs =
   do
      frame <- newFrame parent []
      listBox <- newListBox frame [value ([] :: [String]),bg "white"]

      pack listBox [Side AtLeft,Fill Y]

      scroll <- newScrollBar frame []
      pack scroll [Side AtRight,Fill Y]

      listBox # scrollbar Vertical scroll
      listBox # selectMode Extended

      contentsMVar <- newMVar []

      let
         simpleListBox = SimpleListBox {
            frame = frame,
            listBox = listBox,
            mkString = mkString,
            contentsMVar = contentsMVar
            }

      configure simpleListBox configs

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

getItems :: SimpleListBox value -> IO [value]
getItems simpleListBox =
   do
      contents <- readMVar (contentsMVar simpleListBox)
      return (map val contents)

bindSelection :: SimpleListBox val
   -> IO (Event [SimpleListBoxItem val],IO ())
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
                     Nothing -> []
                     Just items ->
                        let
                           max = length contents0
                        in
                           mapMaybe
                              (\ index -> if index >= max
                                 then
                                    Nothing
                                    -- can happen if events and a deletion
                                    -- get processed in the wrong order.
                                 else
                                    Just (contents0 !! index)
                                 )
                              items
                     )
      return (event,terminator)
