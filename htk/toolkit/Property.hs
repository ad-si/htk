{- --------------------------------------------------------------------
 -
 - Module: Property
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module Property (

  Prop,
  HasProp(..),
  HasProperty(..),
  newProp

) where

import HTk
import ReferenceVariables

data Prop a = Prop (Channel a) (Ref a)

class HasProp i a where                              -- items just need to
  getProp :: i -> Prop a                               -- instantiate this

instance HasProp i a => HasProperty i a where
  set i p = let Prop q v = getProp i
            in syncNoWait (send q p) >> setRef v p
  changed i = let Prop q _ = getProp i in q
  get i = let Prop _ v = getProp i in getRef v

newProp :: a -> IO (Prop a)
newProp a =
  do
    q <- newChannel
    v <- newRef a
    return (Prop q v)

class HasProperty i p where
  set :: i -> p -> IO ()
  changed :: i -> Channel p
  get :: i -> IO p
