{- We implement the Source type and combinators for it. -}
module Sources(
   Source, 
      -- A Source x d represents something that stores a value of
      -- type x and sends change messages of type d.

      -- We instance CanAddSinks (Source x d) x d

   Client,
      -- A Client d is something that consumes change messages of type d.

   -- Producer side
   staticSource, -- :: x -> Source x d
      -- returns a source which never changes

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
      --    -> Source x d1 -> Source (x,state) d2

   -- Combinators
   choose,
      -- :: Source x1 d1 -> Source x2 d2 -> Source (x1,x2) (Either d1 d2)
   seqSource,
      -- :: Source x1 x1 -> (x1 -> Source x2 d2) -> Source x2 d2   

   -- Monadic Sources
   SimpleSource(..), 
      -- newtype for Source x x
      -- Instance of Functor and Monad

   staticSimpleSource, -- :: x -> SimpleSource x

   -- We also instance CanAddSinks (SimpleSource x) x x.
   -- This is done via the following class
   HasSource(..),
   HasSimpleSource(..),

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
   ) where

import Maybe

import Concurrent
import IOExts

import Sink

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

---
-- attachClientTemporary is like attach, but additionally returns an
-- IO action which can be used to prevent any client being run after that
-- IO action is called.
attachClientTemporary :: Client d -> Source x d -> IO (x,IO ())
attachClientTemporary client source =
   do
      (newClient,terminator) <- mkTemporaryClient client
      x <- attachClient newClient source
      return (x,terminator)

---
-- mkTemporaryClient is used to map the client by attachClientTemporary.
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

---
-- mkComputedClient computes a client using a value to be supplied via the
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

---
-- mkStaticClient is used by various functions to create from a client
-- a single static client which tracks its state using an MVar.
mkStaticClient :: Client d -> IO (Client d)
mkStaticClient client =
   do
      (newClient,_) <- mkStaticClientGeneral client
      return newClient

---
-- mkStaticClientGeneral is like mkStaticClient except that it also returns
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
   -> Source x d1 -> Source (x,state) d2
foldSource (xFn :: x -> state) (foldFn :: state -> d1 -> (state,d2))
      ((Source addClient1) :: Source x d1) =
   let
      addClient2 :: Client d2 -> IO (x,state)
      addClient2 client2 =
         do
            let
               createClient :: state -> Client d1  
               createClient state = foldClient state foldFn client2
            (computedClient,writeState) <- mkComputedClient createClient
            x <- addClient1 computedClient
            let
               state = xFn x
            writeState state
            return (x,state)
   in
      Source addClient2

foldClient :: state -> (state -> d1 -> (state,d2)) -> Client d2 -> Client d1
foldClient state1 foldFn (Client clientFn2) =
   let
      clientFn1 d1 =
         do
            let
               (state2,d2) = foldFn state1 d1
            (newClient2Opt) <- clientFn2 d2
            return (fmap
               (foldClient state2 foldFn)
               newClient2Opt
               )
   in
      Client clientFn1

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
         
seqSource (source1 :: Source x1 x1) (getSource2 :: x1 -> Source x2 d2) =
   let
      addClient client2 =
         do
            (staticClient2,clientRunning) <- mkStaticClientGeneral client2

            let
               getClient1 :: (IO (),x1) -> Client x1
               getClient1 (oldTerminator,x1) =
                  let
                     source2 = getSource2 x1

                     client1 terminator = Client (clientFn1 terminator)

                     clientFn1 oldTerminator x1 =
                        do
                           oldTerminator
                           continue <- clientRunning
                           if continue
                              then
                                 do
                                    (x2,newTerminator) 
                                       <- attachClientTemporary 
                                             staticClient2 source2
                                    return (Just (client1 newTerminator))
                              else
                                 return Nothing
                  in
                     client1 oldTerminator
                          
            (client1',write) <- mkComputedClient getClient1
            x1 <- attachClient client1' source1
            let
               source2 = getSource2 x1
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

instance Functor SimpleSource where
   fmap mapFn (SimpleSource source) = 
      SimpleSource ( (map1 mapFn) . (map2 mapFn) $ source)

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

   readContents hasSource =
      do
         let
            client = Client clientFn

            clientFn _ = return Nothing
         attachClient client (toSource hasSource)

-- -----------------------------------------------------------------
-- Other handy utilities
-- -----------------------------------------------------------------

---
-- Pair two SimpleSource's.  This is probably better than using >>=, since it
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

---
-- Does a similar job to pairSimpleSources, so that the sources run
-- parallel.
sequenceSimpleSource :: [SimpleSource x] -> SimpleSource [x]
sequenceSimpleSource [] = return []
sequenceSimpleSource (first:rest) =
   fmap (uncurry (:)) (pairSimpleSources first (sequenceSimpleSource rest))

---
-- For each update d, pairs it with its predecessor (given first).
-- For the very first update, a value is given based on the initial x,
-- mapped by the given function.
mkHistorySource :: (x -> d) -> Source x d -> Source x (d,d)
mkHistorySource getD source =
   map1 (\ (x,d) -> x) (foldSource getD (\ lastD d -> (d,(lastD,d))) source)

---
-- Like mkHistorySource but for SimpleSource's; the x returns the initial
-- value to compare with.
mkHistorySimpleSource :: x -> SimpleSource x -> SimpleSource (x,x)
mkHistorySimpleSource lastX (SimpleSource source) =
   SimpleSource (map1 (\ x -> (lastX,x)) (mkHistorySource id source))

---
-- filter out consecutive duplicates
uniqSimpleSource :: Eq x => SimpleSource x -> SimpleSource x
uniqSimpleSource (SimpleSource source0) =
   let
      source1 = mkHistorySource id source0
      source2 = filter2 (\ (lastD,d) -> if lastD == d then Nothing else Just d)
         source1
   in
      SimpleSource source2