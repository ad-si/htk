{- A toggle is a switch initially True, which can only be made false
   (when some action is performed, say).  This module implements
   toggles, allowing atomic switching to false of 1 toggle, or
   2 toggles together.  To avoid deadlock we use a supply of unique
   integers.
   -}
module Toggle(
   Toggle, -- toggle type
   newToggle, -- create a new toggle
   toggle1, -- toggle 1 toggle from True to False, returning original
   toggle2 -- if toggles are both true, change them to false, otherwise
   -- leave the toggle settings unchanged and return them.
   ) where

import qualified Concurrent
import qualified IOExts(unsafePerformIO)

data Toggle = Toggle Unique (Concurrent.MVar Bool)

newToggle :: IO Toggle
newToggle = 
   do
      uniqVal <- unique
      switch <- Concurrent.newMVar True
      return (Toggle uniqVal switch)

toggle1 :: Toggle -> IO Bool
-- switch bool to false, returning original value.
toggle1 (Toggle _ switch) =
   do
      oldVal <- Concurrent.takeMVar switch
      Concurrent.putMVar switch True
      return oldVal

toggle2 :: (Toggle,Toggle) -> IO(Maybe(Bool,Bool))
-- switch both MVars, WHICH SHOULD BE DIFFERENT, to False, if they are
-- both initially true and return Nothing.  Otherwise return the initial 
-- values.
toggle2 (Toggle unique1 switch1,Toggle unique2 switch2) =
   if switch1 == switch2
      then
         fail "Attempt to toggle2 on the same switch in Toggle.toggle2"
-- At the time of writing (09/02/2000) this can happen if channel
-- send and receive events on the same channel are combined into one
-- event and that event is sync'd on.  Since it is not easy to see
-- what to do in this case, we outlaw it and make it fail noisly.
-- See also TwoWayChannel.hs
      else
         if unique1 < unique2
            then
               orderedToggle switch1 switch2
            else
               do
                  result <- orderedToggle switch2 switch1
                  case result of
                     Just(res1,res2) -> return(Just(res2,res1))
                     Nothing -> return Nothing
   where 
      orderedToggle :: Concurrent.MVar Bool -> Concurrent.MVar Bool ->
            IO (Maybe(Bool,Bool))
      -- In some fixed linear ordering, the arguments to orderedToggle
      -- should be in order.
      orderedToggle switch1 switch2 =
         do
            oldVal1 <- Concurrent.takeMVar switch1
            oldVal2 <- Concurrent.takeMVar switch2
            let 
               ((newVal1,newVal2),result) =
                  if (oldVal1 && oldVal2) 
                     then -- toggle
                        ((False,False),Nothing)
                     else
                        let
                           opair = (oldVal1,oldVal2)
                        in
                           (opair,Just opair)
            Concurrent.putMVar switch1 newVal1
            Concurrent.putMVar switch2 newVal2
            return result
 

{- To fix a possible deadlock we need a supply of unique ordered things.
   -}
type Unique = Int

uniqueSource :: Concurrent.MVar Int
uniqueSource = IOExts.unsafePerformIO(Concurrent.newMVar 0)

unique :: IO Int
unique =
   do
      next <- Concurrent.takeMVar uniqueSource
      Concurrent.putMVar uniqueSource (next+1)
      return next 



