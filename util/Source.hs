{- A SimpleSource is simply the simplest thing that can be attached to a Sink.
   Its state is the last delta sent. 

   A Source can be obtained from a SimpleSource but may also be mapped. -}
module Source(
   SimpleSource,
   newSimpleSource,

   sendSimpleSource,

   Source,
   mkSource,
   ) where

import Sink
import Broadcaster


-- ----------------------------------------------------------------
-- SimpleSource
-- ----------------------------------------------------------------

newtype SimpleSource x = SimpleSource (Broadcaster x x)

newSimpleSource :: x -> IO (SimpleSource x)
newSimpleSource x = 
   do
      broadcaster <- newBroadcaster const x
      return (SimpleSource broadcaster)

sendSimpleSource :: SimpleSource x -> x -> IO ()
sendSimpleSource (SimpleSource broadcaster) x = updateBroadcaster broadcaster x

instance CanAddSinks (SimpleSource x) x x where
   addOldSink (SimpleSource broadcaster) sink = addOldSink broadcaster sink
   readContents (SimpleSource broadcaster) = readContents broadcaster


-- ----------------------------------------------------------------
-- Source
-- ----------------------------------------------------------------

data Source x = Source {
   addOldS :: Sink x -> IO x,
   readC :: IO x
   }

mkSource :: SimpleSource x -> Source x
mkSource simpleSource =
   Source {
      addOldS = (\ sink -> addOldSink simpleSource sink),
      readC = readContents simpleSource
      }

instance CanAddSinks (Source x) x x where
   addOldSink source sink = addOldS source sink
   readContents source = readC source

instance Functor Source where
   fmap f (Source {addOldS = addOldS0,readC = readC0}) =
      let
         addOldS1 sink = 
            do
               x <- addOldS0 (coMapSink f sink)
               return (f x)
         readC1 =
            do
               x <- readC0 
               return (f x)
      in
         Source {addOldS = addOldS1,readC = readC1}
