{- --------------------------------------------------------------------
 -
 - Module: GenGUI
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module GenGUI (

  newGenGUI
  NewItem(..),
  ItemIcon,
  CItem,
  HasProperty

) where

import Item
import Property
import HTk
import Concurrency
import TreeList
import DragAndDrop

