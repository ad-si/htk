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
import RVar
import Concurrency
import Channels

data Prop a = Prop (MsgQueue a) (RVar a)

class HasProp i a where                                 -- items just need to
  getProp :: i -> Prop a                                -- instantiate this

instance HasProp i a => HasProperty i a where
  set i p = let Prop q v = getProp i
            in sendIO q p >> setVar v p

  changed i = let Prop q _ = getProp i
              in lift (receive q)

  get i = let Prop _ v = getProp i
          in getVar v

newProp :: a -> IO (Prop a)
newProp a =
  do
    q <- newMsgQueue
    v <- newRVar a
    return (Prop q v)

class HasProperty i p where
  set :: i -> p -> IO ()
  changed :: i -> IA p
  get :: i -> IO p
