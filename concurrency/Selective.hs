{- #########################################################################

MODULE        : Selective
AUTHOR        : George Russell
based on code written by
                Walter Norzel
		Einar W. Karlsen,  
		University of Bremen
DATE          : 2000
VERSION	      : 0.2
DESCRIPTION   : Interface for the rest of UniForM to first-class 
                composable events, as described in Einar's thesis,
                section 6.4-6.5.8.  The complete set of modules involved
                are (from bottom-up)
      
                   Toggle      boolean switches which can only go True->False
                   PrimEvent   the most primitive notion of event.
                               Look at this if you want a completely new
                               sort of event which can't be simulated by
                               channels
                   BaseEvent   PrimEvent's with continuations
                   Event       The "Event" interface
                   EV          The EV type which uses BaseEvents to
                               implement the Event interface
                   TwoWayChannel  Channels with handshaking passing data
                               both ways, implementing PrimEvent
                   Channels    Simplifies TwoWayChannels only passing data
                               one way, and turns them into EVs.  Provides
                               a choice of Channel's (which require 
                               handshaking) and MsgQueue's (which don't;
                               IE if you send a value, it returns
                               immediately).
                   Selective   (This file).  Imports everything so providing
                               approximately the same interface
                               as in the original UniForM, and does a little
                               unfinished business, mainly things which
                               require both EVs and Channels.
HISTORY       : The first versions of this code (Selective.old.typesafe.hs
                and Selective.old.typeunsafe.hs) were all in one file
                (not in the above modules), very badly structured and hard
                to understand and had a bug.  I have therefore completely
                reimplemented virtually everything, except the Event
                interface itself.

   ######################################################################### 
   -}


module Selective (
	module Event,

	HasReceiveEV(..),
	HasSendEV(..),
	HasReceiveIO(..),
	HasSendIO(..),
	Event(..),
	
	EV,

	Channel,
	newChannel,

	MsgQueue,
	newMsgQueue,

	delay,
	timeout,
	withTimeout,
	deadlock,

	event,
	always,
	promise,

	whenGuard,
	unlessGuard,

        EVSet,
        getEVSetEvent,
        emptyEVSet,
        addEVSet	
	) where

import qualified Computation
import Thread

import Event
import EV
import Channels

-- --------------------------------------------------------------------------
-- Promise
-- --------------------------------------------------------------------------


-- (Tries a computation in another thread.  The try/propagate wraps
-- up any IOErrors to make sure they occur in the proper place)
promise :: IO a -> IO (EV a)
promise beh = do
   ch <- newChannel
   forkIOquiet "Selective.promise" (try beh >>= sync . (send ch))
   return (receive ch >>>= Computation.propagate)

-- --------------------------------------------------------------------------
-- Timeouts 
-- --------------------------------------------------------------------------

timeout :: Duration -> EV ()
timeout = event . promise . delay

withTimeout :: Duration -> EV a -> EV (Maybe a)
withTimeout d ev = 
      timeout d >>> return Nothing 
   +> ev >>>= return . Just


-- --------------------------------------------------------------------------
--  Always
-- --------------------------------------------------------------------------

always :: a -> EV a
always = event . promise . return

-- -------------------------------------------------------------------------
-- EV Sets
-- -------------------------------------------------------------------------
--
-- An EV Set is a set (strictly speaking a family, since the same
-- event can occur more than once) of events.   Like a number of events
-- joined with +>, an EV Set occurs when any of the constituent events occurs.
-- The difference is in the result of the event; the EV Set not only
-- returns the value returned by the constituent event, but also returns
-- the event set of the remaining events.
--
-- NB - EVSets only use operations defined in the Event interface.  So it
-- should be easy to generalise to 
-- instance Event ev => EVSet ev
-- where EVSet implements the following operations.
-- 
newtype EVSet a = EVSet (EV (a,EVSet a))

getEVSetEvent :: EVSet a -> EV (a,EVSet a)
getEVSetEvent (EVSet evSet) = evSet

emptyEVSet :: EVSet a
emptyEVSet = EVSet inaction

-- addEVSet is how new elements get into an EVSet.
addEVSet :: EV a -> EVSet a -> EVSet a
addEVSet new already =
   EVSet (
         (getEVSetEvent already) >>>=
            (\ (value,remaining) -> return (value,addEVSet new remaining))
      +> new >>>= 
            (\ value -> return (value,already))
      )





