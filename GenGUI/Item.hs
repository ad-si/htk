{- --------------------------------------------------------------------
 -
 - Module: Item
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module Item (

  Name,
  ItemIcon,
  CItem,
  NewItem(..)

) where

import Property
import HTk
import Image

data Name = Name { short :: Int -> String,
	           full  :: String }

type ItemIcon = Image

class (HasProperty ItemIcon i, HasProperty Name i) => CItem i

data NewItem =
    forall i . CItem i => FolderItem i [NewItem]
  | forall i . CItem i => LeafItem i
