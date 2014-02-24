{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description: Simple Events
--
-- We implement the Source type and combinators for it.
module Util.Sources(
   Source,
      -- A Source x d represents something that stores a value of
      -- type x and sends change messages of type d.

      -- We instance CanAddSinks (Source x d) x d

   Client,
      -- A Client d is something that consumes change messages of type d.

   -- Producer side
   staticSource, -- :: x -> Source x d
      -- returns a source which never changes

   staticSourceIO, -- :: IO x -> Source x d
      -- returns a source which never changes but gets its initial value
      -- from an IO action.

   variableSource, -- :: x -> IO (Source x d,(x -> (x,[d])) -> IO ())
      -- returns a source which can change.  The supplied action
      -- changes it.

   variableGeneralSource,
      -- :: x -> IO (Source x d,Updater x d)
      -- Like variableSource, but allows the provider of new values to
      -- get out an extra value.  For this it is necessary to go
      -- via the Updater type.

   Updater,
   applyToUpdater, -- :: Updater x d -> (x -> (x,[d],extra)) -> IO extra

   -- Client side
   attachClient, -- :: Client d -> Source x d -> IO x

   -- Transformers
   map1,
      -- :: (x1 -> x2) -> Source x1 d -> Source x2 d

   map1IO,
      -- :: (x1 -> IO x2) -> Source x1 d -> Source x2 d

   map2,
      -- :: (d1 -> d2) -> Source x d1 -> Source x d2
   filter2,
      -- :: (d1 -> Maybe d2) -> Source x d1 -> Source x d2

   filter2IO,
      -- :: (d1 -> IO (Maybe d2)) -> Source x d1 -> Source x d2
      -- To be used with care, since the IO action ties up the source.

   foldSource,
      -- :: (x -> state) -> (state -> d1 -> (state,d2))
      --    -> Source x d1 -> Source (state,x) d2

   foldSourceIO,
      -- :: (x1 -> IO (state,x2)) -> (state -> d1 -> IO (state,d2))
      -- -> Source x1 d1 -> Source (state,x2) d2

   stepSource,
      -- :: (x -> d2) -> (d1 -> d2) -> Source x d1 -> Source x d2
      -- This modifies the source so that whenever we attempt to read from it,
      -- the current "x" value is BOTH returned AND converted to an instant
      -- update (via the first function).

   -- Combinators
   choose,
      -- :: Source x1 d1 -> Source x2 d2 -> Source (x1,x2) (Either d1 d2)
   seqSource,
      -- :: Source x1 x1 -> (x1 -> Source x2 x2) -> Source x2 x2
   flattenSource,
      -- :: Source x [d] -> Source x d
      -- A Source combinator which "flattens" lists of updates.

   -- Monadic Sources
   SimpleSource(..),
      -- newtype for Source x x
      -- Instance of Functor and Monad

   staticSimpleSource, -- :: x -> SimpleSource x

   staticSimpleSourceIO, -- :: IO x -> SimpleSource x

   -- We also instance CanAddSinks (SimpleSource x) x x.
   -- This is done via the following class
   HasSource(..),
   HasSimpleSource(..),

   readContents,
      -- :: HasSource source x d => source -> IO x
      -- Get the current contents of the source, but don't specify any other
      -- action.

   -- miscellaneous handy utilities,
   mkHistorySource, -- :: (x -> d) -> Source x d -> Source x (d,d)
   mkHistorySimpleSource, -- :: x -> SimpleSource x -> SimpleSource (x,x)
   uniqSimpleSource, -- :: Eq x => SimpleSource x -> SimpleSource x

   pairSimpleSources,
      -- :: SimpleSource x1 -> SimpleSource x2 -> SimpleSource (x1,x2)
      -- Pair two SimpleSource's.  This is probably better than using >>=,
      -- since it does not require reregistering with the second SimpleSource

   sequenceSimpleSource, -- :: [SimpleSource x] -> SimpleSource [x]
   -- Does a similar job to pairSimpleSources, so that the sources run
   -- parallel.

   change1, -- :: SimpleSource x -> x -> SimpleSource x
   -- replaces the first value of the SimpleSource.

   mapIOSeq,
      -- :: SimpleSource a -> (a -> IO (SimpleSource b)) -> SimpleSource b
      -- allow us to sequence a SimpleSource where the continuation function
      -- uses an IO action.

   addNewSourceActions,
      -- :: Source x d -> (x -> IO ()) -> (d -> IO ())
      -- -> SinkID -> ParallelExec -> IO x
   -- Run the specified actions for the source, using the given SinkID and
   -- in the ParallelExec thread.
   -- The x -> IO () action is guaranteed to be performed before any of the
   -- d -> IO () actions.

   traceSimpleSource,
      -- :: (a -> String) -> SimpleSource a -> SimpleSource a
      -- Outputs information about what comes through the source, turning
      -- it into a String with the supplied function.  (This is done once
      -- for each active client.)

   traceSource,
      -- :: (a -> String) -> (d -> String) -> Source a d -> Source a d
      -- Like traceSimpleSource but for Source's.

   noLoopSimpleSource,
      -- :: TSem -> ([String] -> a) -> SimpleSource a -> SimpleSource a
      -- Used when we are worried that a SimpleSource recursively constructed
      -- by mapIOSeq, >>= and friends may actually try to call itself, and
      -- so loop forever.   The Strings identify the SimpleSource,
      -- and so the [String] is effectively a backtrace of the TSems,
      -- revealing what chain of simple sources might have caused the loop.

   mkIOSimpleSource,
      -- :: IO (SimpleSource a) -> SimpleSource a

   foldSimpleSourceIO,
      -- :: (x1 -> IO (state,x2)) -> (state -> x1 -> IO (state,x2))
      -- -> SimpleSource x1 -> SimpleSource x2

   ) where

import Data.Maybe

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef

import Util.ExtendedPrelude(HasMapIO(..))
import Util.Sink
import Util.TSem
import Util.Debug(debug)

-- -----------------------------------------------------------------
-- Datatypes
-- -----------------------------------------------------------------

newtype Source x d = Source (Client d -> IO x)

newtype Client d = Client (d -> IO (Maybe (Client d)))

data SourceData x d = SourceData {
   x :: x,
   client :: Maybe (Client d)
   }

-- -----------------------------------------------------------------
-- Producer side
-- -----------------------------------------------------------------

staticSource :: x -> Source x d
staticSource x = Source (\ _ -> return x)


staticSourceIO :: IO x -> Source x d
staticSourceIO action = Source (\ _ -> action)

variableSource :: x -> IO (Source x d,(x -> (x,[d])) -> IO ())
variableSource x =
   do
      mVar <- newMVar (SourceData {
         x = x,
         client = Nothing
         })
      let
         update updateFn =
            do
               (SourceData {x = x1,client = clientOpt}) <- takeMVar mVar
               let
                  (x2,ds) = updateFn x1

                  sendUpdates (Just (Client clientFn)) (d:ds) =
                     do
                        newClientOpt <- clientFn d
                        sendUpdates newClientOpt ds
                  sendUpdates clientOpt _ = return clientOpt

               newClientOpt <- sendUpdates clientOpt ds
               putMVar mVar (SourceData {x = x2,client = newClientOpt})
         addClient newClient =
            do
               (SourceData {x = x,client = oldClientOpt}) <- takeMVar mVar
               let
                  fullNewClient = case oldClientOpt of
                     Nothing -> newClient
                     Just oldClient -> combineClients oldClient newClient
               putMVar mVar (SourceData {x = x,client = Just fullNewClient})
               return x
      return (Source addClient,update)


newtype Updater x d = Updater (forall extra . (x -> (x,[d],extra)) -> IO extra)

applyToUpdater :: Updater x d -> (x -> (x,[d],extra)) -> IO extra
applyToUpdater (Updater update) updateAct = update updateAct

variableGeneralSource :: x -> IO (Source x d,Updater x d)
variableGeneralSource x =
   do
      mVar <- newMVar (SourceData {
         x = x,
         client = Nothing
         })
      let
         update updateFn =
            do
               (SourceData {x = x1,client = clientOpt}) <- takeMVar mVar

               let
                  (x2,ds,extra) = updateFn x1
                  sendUpdates (Just (Client clientFn)) (d:ds) =
                     do
                        newClientOpt <- clientFn d
                        sendUpdates newClientOpt ds
                  sendUpdates clientOpt _ = return clientOpt

               newClientOpt <- sendUpdates clientOpt ds
               putMVar mVar (SourceData {x = x2,client = newClientOpt})
               return extra
         addClient newClient =
            do
               (SourceData {x = x,client = oldClientOpt}) <- takeMVar mVar
               let
                  fullNewClient = case oldClientOpt of
                     Nothing -> newClient
                     Just oldClient -> combineClients oldClient newClient
               putMVar mVar (SourceData {x = x,client = Just fullNewClient})
               return x
      return (Source addClient,Updater update)

combineClients :: Client d -> Client d -> Client d
combineClients (Client clientFn1) (Client clientFn2) =
   let
      clientFn d =
         do
            newClient1Opt <- clientFn1 d
            newClient2Opt <- clientFn2 d
            case (newClient1Opt,newClient2Opt) of
               (Nothing,Nothing) -> return Nothing
               (Just newClient1,Nothing) -> return (Just newClient1)
               (Nothing,Just newClient2) -> return (Just newClient2)
               (Just newClient1,Just newClient2)
                  -> return (Just (combineClients newClient1 newClient2))
   in
      Client clientFn

-- -----------------------------------------------------------------
-- Client side
-- -----------------------------------------------------------------

attachClient :: Client d -> Source x d -> IO x
attachClient client (Source addClient) = addClient client

-- | attachClientTemporary is like attach, but additionally returns an
-- IO action which can be used to prevent any client being run after that
-- IO action is called.
attachClientTemporary :: Client d -> Source x d -> IO (x,IO ())
attachClientTemporary client source =
   do
      (newClient,terminator) <- mkTemporaryClient client
      x <- attachClient newClient source
      return (x,terminator)

-- | mkTemporaryClient is used to map the client by attachClientTemporary.
mkTemporaryClient :: Client d -> IO (Client d,IO ())
mkTemporaryClient client =
   do
      ioRef <- newIORef True -- write False to this to stop the client.
      let
         newClient client = Client (newClientFn client)

         newClientFn (Client oldClientFn) d  =
            do
               goAhead <- readIORef ioRef
               if goAhead
                  then
                     do
                        newClientOpt <- oldClientFn d
                        return (fmap newClient newClientOpt)
                  else
                     return Nothing
      return (newClient client,writeIORef ioRef False)

-- | mkComputedClient computes a client using a value to be supplied via the
-- returned function.  (Hopefully soon after, because of course the source
-- will block until it is.)
mkComputedClient :: (x -> Client d) -> IO (Client d,x -> IO ())
mkComputedClient getClient =
   do
      mVar <- newEmptyMVar
      let
         client = Client clientFn

         clientFn d =
            do
               x <- takeMVar mVar
               let
                  (Client realClientFn) = getClient x
               realClientFn d
      return (client,putMVar mVar)

-- | mkComputedClient is like mkComputedClient, but still more dangerously
-- allows an IO action to compute the client.
--
-- It also allows the supplied function to provide Nothing, indicating no
-- client.
mkComputedClientIO :: (x -> IO (Maybe (Client d))) -> IO (Client d,x -> IO ())
mkComputedClientIO getClient =
   do
      mVar <- newEmptyMVar
      let
         client = Client clientFn

         clientFn d =
            do
               x <- takeMVar mVar
               clientOpt <- getClient x
               case clientOpt of
                  Nothing -> return Nothing
                  Just (Client realClientFn) -> realClientFn d
      return (client,putMVar mVar)

-- | mkStaticClient is used by various functions to create from a client
-- a single static client which tracks its state using an MVar.
mkStaticClient :: Client d -> IO (Client d)
mkStaticClient client =
   do
      (newClient,_) <- mkStaticClientGeneral client
      return newClient

-- | mkStaticClientGeneral is like mkStaticClient except that it also returns
-- an action which determines if the client is still running.
mkStaticClientGeneral :: Client d -> IO (Client d,IO Bool)
mkStaticClientGeneral (client :: Client d) =
   do
      mVar <- newMVar (Just client)
      let
         client = Client clientFn

         clientFn d =
            do
               clientOpt <- takeMVar mVar
               case clientOpt of
                  Nothing -> do
                     putMVar mVar clientOpt
                     return Nothing
                  Just (Client clientFnInner) ->
                     do
                        newClientOpt <- clientFnInner d
                        putMVar mVar newClientOpt
                        return (Just client)

         clientRunning =
            do
               clientOpt <- readMVar mVar
               return (isJust clientOpt)

      return (client,clientRunning)

-- -----------------------------------------------------------------
-- Transformers
-- -----------------------------------------------------------------

map1 :: (x1 -> x2) -> Source x1 d -> Source x2 d
map1 mapFn (Source addClient1) =
   let
      addClient2 d =
         do
            x1 <- addClient1 d
            return (mapFn x1)
   in
      Source addClient2

map1IO :: (x1 -> IO x2) -> Source x1 d -> Source x2 d
map1IO mapFn (Source addClient1) =
   let
      addClient2 d =
         do
            x1 <- addClient1 d
            mapFn x1
   in
      Source addClient2

map2 :: (d1 -> d2) -> Source x d1 -> Source x d2
map2 mapFn (Source addClient1) =
   let
      addClient2 newClient1 = addClient1 (coMapClient mapFn newClient1)
   in
      Source addClient2

coMapClient :: (d1 -> d2) -> Client d2 -> Client d1
coMapClient mapFn (Client clientFn2) =
   let
      client1 = Client clientFn1

      clientFn1 d1 =
         do
            let
               d2 = mapFn d1
            newClient2Opt <- clientFn2 d2
            return (fmap
               (coMapClient mapFn)
               newClient2Opt
               )
   in
      client1

filter2 :: (d1 -> Maybe d2) -> Source x d1 -> Source x d2
filter2 filterFn (Source addClient1) =
   let
      addClient2 newClient1 = addClient1 (filterClient filterFn newClient1)
   in
      Source addClient2

filterClient :: (d1 -> Maybe d2) -> Client d2 -> Client d1
filterClient filterFn (Client clientFn2) =
   let
      client1 = Client clientFn1

      clientFn1 d1 =
         let
            d2Opt = filterFn d1
         in
            case d2Opt of
               Nothing -> return (Just client1)
               Just d2 ->
                  do
                     newClient2Opt <- clientFn2 d2
                     return (fmap
                        (filterClient filterFn)
                        newClient2Opt
                        )
   in
      client1

filter2IO :: (d1 -> IO (Maybe d2)) -> Source x d1 -> Source x d2
filter2IO filterFn (Source addClient1) =
   let
      addClient2 newClient1 = addClient1 (filterClientIO filterFn newClient1)
   in
      Source addClient2

filterClientIO :: (d1 -> IO (Maybe d2)) -> Client d2 -> Client d1
filterClientIO filterFn (Client clientFn2) =
   let
      client1 = Client clientFn1

      clientFn1 d1 =
         do
            d2Opt <- filterFn d1
            case d2Opt of
               Nothing -> return (Just client1)
               Just d2 ->
                  do
                     newClient2Opt <- clientFn2 d2
                     return (fmap
                        (filterClientIO filterFn)
                        newClient2Opt
                        )
   in
      client1

foldSource :: (x -> state) -> (state -> d1 -> (state,d2))
   -> Source x d1 -> Source (state,x) d2
foldSource xFn foldFn =
   let
      xFnIO x = return (xFn x,x)
      foldFnIO state d = return (foldFn state d)
   in
      foldSourceIO xFnIO foldFnIO

-- | Fold a Source so that it can carry state around.
foldSourceIO :: (x1 -> IO (state,x2)) -> (state -> d1 -> IO (state,d2))
   -> Source x1 d1 -> Source (state,x2) d2
foldSourceIO (xFnIO :: x1 -> IO (state,x2))
      (foldFnIO :: state -> d1 -> IO (state,d2))
      ((Source addClient1) :: Source x1 d1) =
   let
      addClient2 :: Client d2 -> IO (state,x2)
      addClient2 client2 =
         do
            let
               createClient :: state -> Client d1
               createClient state = foldClientIO state foldFnIO client2
            (computedClient,writeState) <- mkComputedClient createClient
            x1 <- addClient1 computedClient

            (state,x2) <- xFnIO x1
            writeState state
            return (state,x2)
   in
      Source addClient2

foldClientIO
   :: state -> (state -> d1 -> IO (state,d2)) -> Client d2 -> Client d1
foldClientIO state1 foldFnIO (Client clientFn2) =
   let
      clientFn1 d1 =
         do
            (state2,d2) <- foldFnIO state1 d1
            (newClient2Opt) <- clientFn2 d2
            return (fmap
               (foldClientIO state2 foldFnIO)
               newClient2Opt
               )
   in
      Client clientFn1

stepSource :: (x -> d2) -> (d1 -> d2) -> Source x d1 -> Source x d2
stepSource fromX fromD (Source addClient1) =
   let
      addClient2 (Client clientFn2) =
         do
            (computedClient,writeClientOpt) <- mkComputedClientIO return
            x <- addClient1 ((coMapClient fromD) computedClient)
            clientOpt <- clientFn2 (fromX x)
            writeClientOpt clientOpt
            return x
   in
      Source addClient2

-- | A Source combinator which \"flattens\" lists of updates.
flattenSource :: Source x [d] -> Source x d
flattenSource (Source addClient1) =
   let
      addClient2 client1 = addClient1 (flattenClient client1)
   in
      (Source addClient2)

flattenClient :: Client d -> Client [d]
flattenClient client0 = Client (mkClientFn client0)
   where
      mkClientFn :: Client d -> [d] -> IO (Maybe (Client [d]))
      mkClientFn client0 [] = return (Just (flattenClient client0))
      mkClientFn (Client clientFn1) (d:ds) =
         do
            client1Opt <- clientFn1 d
            case client1Opt of
               Nothing -> return Nothing
               Just client2 -> mkClientFn client2 ds

-- -----------------------------------------------------------------
-- Combinators
-- -----------------------------------------------------------------

-- Combinators
choose :: Source x1 d1 -> Source x2 d2 -> Source (x1,x2) (Either d1 d2)
choose ((Source addClient1) :: Source x1 d1)
       ((Source addClient2) :: Source x2 d2) =
   let
      addClient (client :: Client (Either d1 d2)) =
         do
            (Client staticClientFn) <- mkStaticClient client
            let
               client1 = Client clientFn1

               clientFn1 d1 =
                  do
                     continue <- staticClientFn (Left d1)
                     return (fmap (\ _ -> client1) continue)

               client2 = Client clientFn2

               clientFn2 d2 =
                  do
                     continue <- staticClientFn (Right d2)
                     return (fmap (\ _ -> client2) continue)

            x1 <- addClient1 client1
            x2 <- addClient2 client2
            return (x1,x2)
   in
      Source addClient

seqSource :: Source x1 x1 -> (x1 -> Source x2 x2) -> Source x2 x2
seqSource source getSource = seqSourceIO source (\ x1 -> return (getSource x1))

seqSourceIO :: Source x1 x1 -> (x1 -> (IO (Source x2 x2))) -> Source x2 x2
seqSourceIO (source1 :: Source x1 x1) (getSource2 :: x1 -> IO (Source x2 x2)) =
   let
      addClient client2 =
         do
            (staticClient2 @ (Client staticClientFn),clientRunning)
               <- mkStaticClientGeneral client2

            let
               getClient1 :: (IO (),x1) -> Client x1
               getClient1 (oldTerminator,x1) =
                  let
                     client1 terminator = Client (clientFn1 terminator)

                     clientFn1 oldTerminator x1 =
                        do
                           source2 <- getSource2 x1

                           oldTerminator
                           continue <- clientRunning
                           if continue
                              then
                                 do
                                    (staticClient2',write)
                                       <- mkComputedClient
                                          (const staticClient2)

                                    (x2,newTerminator)
                                       <- attachClientTemporary
                                             staticClient2' source2
                                    staticClientFn x2
                                    write ()
                                    return (Just (client1 newTerminator))
                              else
                                 return Nothing
                  in
                     client1 oldTerminator

            (client1',write) <- mkComputedClient getClient1
            x1 <- attachClient client1' source1

            source2 <- getSource2 x1

            (x2,firstTerminator) <- attachClientTemporary staticClient2 source2
            write (firstTerminator,x1)
            return x2
   in
      Source addClient

-- -----------------------------------------------------------------
-- SimpleSource
-- -----------------------------------------------------------------

newtype SimpleSource x = SimpleSource (Source x x)

staticSimpleSource :: x -> SimpleSource x
staticSimpleSource x = SimpleSource (staticSource x)

staticSimpleSourceIO :: IO x -> SimpleSource x
staticSimpleSourceIO act = SimpleSource (staticSourceIO act)

instance Functor SimpleSource where
   fmap mapFn (SimpleSource source) =
      SimpleSource ( (map1 mapFn) . (map2 mapFn) $ source)

instance HasMapIO SimpleSource where
   mapIO mapFn (SimpleSource source) =
      SimpleSource (
         (map1IO mapFn)
         . (filter2IO
            (\ x ->
               do
                  y <- mapFn x
                  return (Just y)
               )
            )
         $ source
         )


mapIOSeq :: SimpleSource a -> (a -> IO (SimpleSource b)) -> SimpleSource b
mapIOSeq (SimpleSource (source1 :: Source a a))
      (getSimpleSource :: (a -> IO (SimpleSource b))) =
   let
      getSource :: a -> IO (Source b b)
      getSource a =
         do
            (SimpleSource source) <- getSimpleSource a
            return source

      source2 :: Source b b
      source2 = seqSourceIO source1 getSource
   in
      SimpleSource source2

instance Applicative SimpleSource where
   pure = return
   (<*>) = ap

instance Monad SimpleSource where
   return x = SimpleSource (staticSource x)
   (>>=) (SimpleSource source1) getSimpleSource2 =
      let
         getSource2 x =
            let
               (SimpleSource source2) = getSimpleSource2 x
            in
               source2
      in
         SimpleSource (seqSource source1 getSource2)

-- -----------------------------------------------------------------
-- The HasSource and HasSimpleSource classes and their instances
-- -----------------------------------------------------------------

class HasSource hasSource x d | hasSource -> x,hasSource -> d where
   toSource :: hasSource -> Source x d

class HasSimpleSource hasSource x | hasSource -> x where
   toSimpleSource :: hasSource -> SimpleSource x

instance HasSource (Source x d) x d where
   toSource source = source

instance HasSimpleSource (SimpleSource x) x where
   toSimpleSource simpleSource = simpleSource

instance HasSource (SimpleSource x) x x where
   toSource (SimpleSource source) = source

-- -----------------------------------------------------------------
-- The readContents function
-- -----------------------------------------------------------------

-- | Get the current contents of the source, but don\'t specify any other
-- action.
readContents :: HasSource source x d => source -> IO x
readContents hasSource =
   let
      trivialClient = Client (\ _ -> return Nothing)
   in
      attachClient trivialClient (toSource hasSource)

-- -----------------------------------------------------------------
-- Instance of CanAddSinks
-- -----------------------------------------------------------------

instance HasSource hasSource x d => CanAddSinks hasSource x d where
   addOldSink hasSource sink =
      do
         let
            client = Client clientFn

            clientFn d =
               do
                  continue <- putSink sink d
                  return (if continue
                     then
                        Just client
                     else
                        Nothing
                     )
         attachClient client (toSource hasSource)

-- -----------------------------------------------------------------
-- Other handy utilities
-- -----------------------------------------------------------------

-- | Pair two SimpleSource\'s.  This is probably better than using >>=, since it
-- does not require reregistering with the second SimpleSource
pairSimpleSources :: SimpleSource x1 -> SimpleSource x2 -> SimpleSource (x1,x2)
pairSimpleSources (SimpleSource source1) (SimpleSource source2) =
   let
      sourceChoose = choose source1 source2
      source =
         foldSource
            id
            (\ (x1,x2) change ->
               let
                  new = case change of
                     Left newX1 -> (newX1,x2)
                     Right newX2 -> (x1,newX2)
               in
                  (new,new)
               )
            sourceChoose
   in
      SimpleSource (map1 fst source)

-- | Does a similar job to pairSimpleSources, so that the sources run
-- parallel.
sequenceSimpleSource :: [SimpleSource x] -> SimpleSource [x]
sequenceSimpleSource [] = return []
sequenceSimpleSource (first:rest) =
   fmap (uncurry (:)) (pairSimpleSources first (sequenceSimpleSource rest))

-- | For each update d, pairs it with its predecessor (given first).
-- For the very first update, a value is given based on the initial x,
-- mapped by the given function.
mkHistorySource :: (x -> d) -> Source x d -> Source x (d,d)
mkHistorySource getD source =
   map1 (\ (d,x) -> x) (foldSource getD (\ lastD d -> (d,(lastD,d))) source)

-- | Like mkHistorySource but for SimpleSource\'s; the x returns the initial
-- value to compare with.
mkHistorySimpleSource :: x -> SimpleSource x -> SimpleSource (x,x)
mkHistorySimpleSource lastX (SimpleSource source) =
   SimpleSource (map1 (\ x -> (lastX,x)) (mkHistorySource id source))

-- | filter out consecutive duplicates
uniqSimpleSource :: Eq x => SimpleSource x -> SimpleSource x
uniqSimpleSource (SimpleSource source0) =
   let
      source1 = mkHistorySource id source0
      source2 = filter2 (\ (lastD,d) -> if lastD == d then Nothing else Just d)
         source1
   in
      SimpleSource source2


-- | Fold a Simple Source, so that it carries state.
-- The state is recomputed for each client.
foldSimpleSourceIO :: (x1 -> IO (state,x2)) -> (state -> x1 -> IO (state,x2))
   -> SimpleSource x1 -> SimpleSource x2
foldSimpleSourceIO (getStateIO :: x1 -> IO (state,x2)) updateStateIO
      (SimpleSource (source :: Source x1 x1)) =
   let
      source1 :: Source (state,x2) x2
      source1 = foldSourceIO getStateIO updateStateIO source
   in
      SimpleSource (map1 snd source1)

-- | replaces the first value of the SimpleSource.
change1 :: SimpleSource x -> x -> SimpleSource x
change1 (SimpleSource source) x = SimpleSource (map1 (\ _ -> x) source)

-- | Run the specified actions for the source, using the given SinkID and
-- in the ParallelExec thread.
-- The x -> IO () action is guaranteed to be performed before any of the
-- d -> IO () actions.
addNewSourceActions :: Source x d -> (x -> IO ()) -> (d -> IO ())
   -> SinkID -> ParallelExec -> IO x
addNewSourceActions (source1 :: Source x d) actionX actionD sinkID parallelX =
   do
      mVar <- newEmptyMVar -- used to return the first x value
      let
         actionX' x =
            do
               putMVar mVar x
               actionX x

         (source2 :: Source x (IO ())) = stepSource actionX' actionD source1
      addNewQuickSinkGeneral
         source2
         (\ action -> parallelExec parallelX action)
         sinkID
      takeMVar mVar

-- -----------------------------------------------------------------
-- Trace functions
-- -----------------------------------------------------------------

-- | Outputs information about what comes through the source, turning
-- it into a String with the supplied function.  (This is done once
-- for each active client.)
traceSimpleSource :: (a -> String) -> SimpleSource a -> SimpleSource a
traceSimpleSource toS (SimpleSource source) =
   SimpleSource (
      (map1IO
         (\ a ->
            do
               putStrLn ("Initialising "++toS a)
               return a
            )
         )
      .
      (filter2IO
         (\ a ->
            do
               putStrLn ("Updating "++toS a)
               return (Just a)
            )
         )
      $
      source
      )

-- | Outputs information about what comes through the source, turning
-- it into a String with the supplied function.  (This is done once
-- for each active client.)
traceSource :: (a -> String) -> (d -> String) -> Source a d -> Source a d
traceSource toS1 toS2 source =
   (map1IO
      (\ a ->
         do
            putStrLn ("Initialising "++toS1 a)
            return a
         )
      )
   .
   (filter2IO
      (\ d ->
         do
            putStrLn ("Updating "++toS2 d)
            return (Just d)
         )
      )
   $
   source

-- -----------------------------------------------------------------
-- noLoop functions.  (Only noLoopSimpleSource is exported, for now.)
-- -----------------------------------------------------------------

noLoopSource :: TSem -> ([String] -> x) -> ([String] -> d)
   -> Source x d -> Source x d
noLoopSource tSem toX toD (Source addClient0 :: Source x d) =
   let
      mkClient :: Client d -> Client d
      mkClient client = Client (mkClientFn client)

      mkClientFn :: Client d -> d -> IO (Maybe (Client d))
      mkClientFn (client @ (Client clientFn0)) d =
         do
            (looped :: Either [String] (Maybe (Client d)))
               <- synchronizeTSem tSem (clientFn0 d)
            case looped of
               Left strings ->
                  do
                     debug ("mkClientFn loop caught " ++ show strings)
                     -- repeat with the artificial d (which had better
                     -- not cause a loop).
                     mkClientFn client (toD strings)
               Right clientOpt -> return (fmap mkClient clientOpt)

      addClient1 :: Client d -> IO x
      addClient1 client =
         do
            stringsOrX <- synchronizeTSem tSem
               (addClient0 (mkClient client))
            case stringsOrX of
               Left strings -> return (toX strings)
               Right x -> return x
   in
      Source addClient1

-- | Used when we are worried that a SimpleSource recursively constructed
-- by mapIOSeq, >>= and friends may actually try to call itself, and
-- so loop forever.   The Strings identify the SimpleSource,
-- and so the [String] is effectively a backtrace of the TSems, revealing what
-- chain of simple sources might have caused the loop.
noLoopSimpleSource :: TSem -> ([String] -> a) -> SimpleSource a
   -> SimpleSource a
noLoopSimpleSource tSem toA (SimpleSource source0) =
   let
      source1 = noLoopSource tSem toA toA source0
   in
      SimpleSource source1

-- ---------------------------------------------------------------------------
-- mkIOSource and mkIOSimpleSource
-- ---------------------------------------------------------------------------

mkIOSource :: IO (Source x d) -> Source x d
mkIOSource act =
   let
      addClient client =
         do
            (Source addClient1) <- act
            addClient1 client
   in
      Source addClient

mkIOSimpleSource :: IO (SimpleSource a) -> SimpleSource a
mkIOSimpleSource act =
   SimpleSource (mkIOSource (
      do
         simpleSource <- act
         return (toSource simpleSource)
      ))
