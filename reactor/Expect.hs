{- #########################################################################

Changed as of October 1999 to use the GHC Regex library rather than
the external Regexp one (which doesn't seem to be maintained much
more).

MODULE        : Expect
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Provides similar functionality as the expect tool documented 
                in

                D. Libes: expect: Scripts for Controlling Interactive 
                   Processes.
                In Computing Systems, Vol 4, No. 2, Spring 1991.

                Improvements over this tools are: full support for 
                concurrency, first class composable event values and pattern 
                priorities.

                The implementation builds upon three components:
                the Regexp regular expression utility of Meurig Sage,
                the dispatcher module for low-level event handling, and the
                interactor framework for application oriented event
                handling. 

                Patterns are consequently provided in terms of first class
                composable event values, which can be combined using the
                various combiantors over events.

                The implementation uses a special adaptor that maintains
                a buffer of yet-to-be-matched characters as well as the
                set of registered patterns. Communication between the
                adaptor and the set of event listener interactors is
                purely synchronous in order to keep the two in sync.

                For the underlying regexp utility see:

                Meuring Sage: Regular Expresion Library for Haskell,
                        University of Glasgow, December 5, 1996.


TO BE DONE  :   It should be possible to run tools like passwd and ftp in the
                background just like for expect. If this kind of service is
                needed there are two solutions: 1) to run expect in the 
                background (easy but inefficent) or 2) to add some extra
                stuff to the current implementation. I'm in favour of (2),
                which explains why the current release does not support
                running ftp and passwd for the time being.


   ######################################################################### -}


module Expect (
   ToolStatus,
   
   Destructible(..),
   Tool(..),
   UnixTool(..),
   CommandTool(..),
   
   Expect,
   newExpect,
   
   PatternDesignator(..),
   
   match,
   expect,
   matchEOF,
   matchLongLine,
   matchLine,
   
   commandFailed
   ) where

import qualified IOExts(unsafePerformIO)
import FiniteMap
import Posix(signalProcess,sigKILL)

import ExtendedPrelude(insertOrd)
import Object
import Maybes
import Dynamics
import Debug(debug)

import Concurrency

import Interaction
import Listener
import ChildProcess
import Interpreter(commandFailed)
import SIMClasses
import Collector
import RegularExpression

-- --------------------------------------------------------------------------
-- Basic types
-- --------------------------------------------------------------------------

data Expect = 
   Expect {
      child           :: ChildProcess,         -- unix server
      sap             :: SAP String (),
      -- The reader sends messages read from the tool by calling
      -- the eventsap; the matcher uses "provide" to process them.
      eofChannel      :: Channel (),           -- eof signal  
      -- the reader sends a message along this channel when it
      -- gets an error.
      regChannel       :: Channel (RST -> IO RST), 
      -- Registration changes.  A registration change is a function
      -- from RST to IO RST which you send along regChannel and which
      -- is read by the matcher thread.  The function had better be
      -- quickly handled.
      collectable     :: Maybe CollectibleObj
      }

data Pattern = Pattern RegularExpression Int String  
-- compiled pattern, priority, original string

data EOF = EOF

data RST = RST [Pattern] (FiniteMap EventID [Listener])
-- RST contains the map from Patterns to channels, arranged in two ways.
-- The list contains all patterns matched.    The FiniteMap indexes
-- them (by EventID), giving the list of listeners to send to, in
-- order of priority.
-- (The FiniteMap can also contain the EOF event.)

data EST = EST String RST
-- This is the state maintained by the matcher thread.  The String
-- is the buffer of previously read characters


-- --------------------------------------------------------------------------
--  Expect Tool
-- --------------------------------------------------------------------------

newExpect :: FilePath -> [Config PosixProcess] -> IO Expect
newExpect tool confs = 
   do 
      sap <- newSAP
      eofChannel <- newChannel
      regChannel <- newChannel
      child <- newChildProcess tool ((linemode False) : confs)
         -- this means the child process runs with linemode False
         -- UNLESS the configurations say otherwise.
      let 
         expect =
            Expect {
               child=child,
               sap=sap,
               eofChannel=eofChannel,
               regChannel=regChannel,
               collectable=Nothing
               }
      forkIO (matcher expect (EST "" emptyRST))
      forkIO (reader expect)
      cobj <- newCollectibleObj
      -- specify that when we want to GC this Expect instance
      -- we destroy the childprocess
      destructor (destroy child) cobj
      return (expect {collectable = Just cobj}) 

-- --------------------------------------------------------------------------
--  Reader Thread.  This reads messages from the child.
--  When it obtains them it communicates them by passing them to
--  the SAP.
-- --------------------------------------------------------------------------

reader :: Expect -> IO ()
reader expect@(Expect{child=child,sap=sap,eofChannel=eofChannel}) = 
   do
      ans <- try(readMsg child)
      case ans of
         Left e  ->  
            do -- I think this means the file was closed
               sendIO eofChannel ()         -- EOF
               closeChildProcessFds child
         Right v ->  
            do
               sync(call sap v)
               reader expect
        
                        
-- --------------------------------------------------------------------------
--  Dispatcher and Adaptor Thread
-- --------------------------------------------------------------------------

matcher :: Expect -> EST -> IO ()
matcher 
      expect@(Expect{sap=sap,eofChannel=eofChannel,regChannel=regChannel}) 
      est@(EST buffer rst) = 
   sync (
         (provide sap 
            (\msg -> 
               -- message received from the tool via "reader".
               do
                  newEst <- attemptMatch expect (EST (buffer ++ msg) rst)
                  return ((),newEst) 
               ) >>>= \ newEst -> matcher expect newEst
            )
      +> registrationChanged regChannel rst >>>=  
           (\ newRst -> 
                do
                   newEst <- attemptMatch expect (EST buffer newRst)
                   matcher expect newEst
                )
      +> whenEV (buffer == "") 
           (receive eofChannel >>> 
              do
                 newEst <- delegateEOF expect est
                 matcher expect newEst
              )
      )

-- attemptMatch finds and handles all the non-overlapping matched
-- patterns in the buffer.
attemptMatch :: Expect -> EST -> IO EST 
attemptMatch expect (EST "" rst)= return (EST "" rst)
attemptMatch expect (EST ('\n':buffer) rst) = 
   attemptMatch expect (EST buffer rst)
attemptMatch expect est@(EST buffer (RST patterns _)) = 
   do
      case (matchPattern patterns buffer) of 
         Nothing -> 
            do
               logNoMatch buffer           
               return est
         Just (pattern,matchResult) -> 
            do
               logMatch buffer pattern matchResult
               delegateEvent expect pattern matchResult est

-- matchPattern patterns str 
-- searches for the first matching pattern in the list of patterns.
-- It returns the corresponding pattern,
-- plus the MatchResult value.
matchPattern :: [Pattern] -> String -> Maybe (Pattern,MatchResult)
matchPattern patterns string =
   firstJust
      (map
         ( \ pattern @ (Pattern regularExpression _ patString) ->
            case matchString regularExpression string of
               Nothing -> Nothing
               Just matchResult -> Just (pattern,matchResult)
            )  
         patterns
         )

{- delegateEvent handles a successful match.
   delegateEvent expect pattern matchResult est
   pattern is the successful match.
   -}
delegateEvent :: Expect -> Pattern -> MatchResult -> EST -> IO EST
delegateEvent expect @ (Expect{regChannel=regChannel}) 
      pattern matchResult est@(EST buffer rst) = 
   sync (
      choose (map (\ listener -> oneway listener eID matchResult) listeners) 
         >>>= -- wait until one of the listeners accepts the event.
            (\ ev -> -- this is the event to wait for for an acknowledgment.
               do
                   newRst <- awaitReply' regChannel ev rst
                   logAck
                   attemptMatch expect (EST (getAfter matchResult) newRst)
               )
      +> registrationChanged regChannel rst >>>= 
         \ newRst -> attemptMatch expect (EST buffer newRst)
      )
  where 
     eID = toEventID (expect,pattern)
     listeners = getListeners eID rst


delegateEOF :: Expect -> EST -> IO EST
-- delegateEOF is similar to delegateEvent but is called when an
-- EOF message is received from the reader.
delegateEOF expect @ (Expect{regChannel=regChannel}) est@(EST buffer rst)= 
   sync (  
         choose (map (\ listener -> oneway listener eID ()) listeners) >>>= 
            (\ ev -> -- this is the event to wait for for an acknowledgment. 
               do
                  newRst <- awaitReply' regChannel ev rst
                  logAck
                  return (EST buffer rst)
               )
      +> registrationChanged regChannel rst >>>=
            (\ newRst ->
               delegateEOF expect (EST buffer newRst)
               )
      )
   where   
      eID = toEventID (expect,EOF)
      listeners = getListeners eID rst

-- --------------------------------------------------------------------------
--  Awaiting replies
-- --------------------------------------------------------------------------

-- awaitReply' regChannel reply est
-- waits for reply to happen while simultaneously
-- monitoring registration changes to update rst
awaitReply' :: Channel (RST -> IO RST) -> EV () -> RST -> IO RST
awaitReply' regChannel reply rst = 
   sync (
         reply >>> return rst
      +> registrationChanged regChannel rst >>>= 
            \ newRst -> awaitReply' regChannel reply newRst
      )

registrationChanged :: Channel (RST -> IO RST) -> RST -> EV RST
registrationChanged regChannel rst = 
   receive regChannel >>>= 
      (\ registrationChange -> registrationChange rst )


-- --------------------------------------------------------------------------
-- We now come to code which enables the user to set up patterns
-- and send messages to the Expect threads.
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- Class Pattern Designators
-- --------------------------------------------------------------------------

class PatternDesignator p where
   toPattern :: p -> Pattern       

instance PatternDesignator [Char] where
   toPattern string = Pattern (compile string) 0 string

instance PatternDesignator ([Char],Int) where
   toPattern (string,priority) = Pattern (compile string) priority string

-- --------------------------------------------------------------------------
--  Instance EventDesignator
-- --------------------------------------------------------------------------

instance EventDesignator (Expect,Pattern) where
   toEventID (expect,pattern @ (Pattern _ prio patString)) = 
      EventID (objectID expect) ('P':((show prio) ++ patString))
-- The map toEventID had better be one-to-one!

instance EventDesignator (Expect,EOF) where
   toEventID (expect,patString) = EventID (objectID expect) ("EOF")

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Expect where
   objectID expect = objectID (child expect)

instance Destructible Expect where
   destroy expect     = destroy (child expect)
   destroyed expect   = destroyed (child expect)    

instance Collectible Expect where
    getCollectibleObj expect = 
       case collectable expect of 
          Just co -> co

instance Tool Expect where
    getToolStatus expect = getToolStatus (child expect)

instance UnixTool Expect where
    getUnixProcessID expect = getUnixProcessID (child expect)  
        

instance CommandTool Expect where 
    execCmd cmd expect = execOneWayCmd cmd expect
    evalCmd cmd expect = error "Expect.evalCmd not implemented"       
    execOneWayCmd cmd expect  = sendMsg (child expect) cmd 


-- --------------------------------------------------------------------------
--  Derived Events
-- --------------------------------------------------------------------------

expect :: PatternDesignator p => Expect -> p -> IA String
expect exp ptn = 
   match exp ptn >>>= 
      ( \ matchResult ->
         return (getMatched matchResult)
         )

matchLongLine:: Expect -> IA String
matchLongLine exp = 
   expect exp "^.*" >>>=
      ( \ matched ->
         sync (matchTail exp matched)
         )
   where  
      matchTail :: Expect -> String -> IA String
      matchTail exp soFar =
            (expect exp ".*" >>>= 
               (\ matched ->
                     sync(matchTail exp (soFar ++ matched)
                     )
                  )
                )
         +> expect exp "\n" >>> return (soFar++"\n")
             

matchLine:: Expect -> IA String
matchLine exp = expect exp "^.*\n" 


-- --------------------------------------------------------------------------
--  EOF Event 
-- --------------------------------------------------------------------------

-- really a simplified version of the match function which follows.
matchEOF :: Expect -> IA ()
matchEOF expect = 
   (interaction eID register deregister :: IA ()) >>> 
      do
        try(getToolStatus expect) -- why ?? 
        done
   where  
      eID = toEventID (expect,EOF)
      Expect {regChannel=regChannel} = expect
      register listener   = 
         sendIO regChannel (registerListener eID listener) 
      deregister 
         listener = sendIO regChannel (deregisterListener eID listener)


-- --------------------------------------------------------------------------
--  Matching Event 
-- --------------------------------------------------------------------------

match :: PatternDesignator p => Expect -> p -> IA MatchResult 
match expect p = interaction eID register unregister 
-- Use match to make an IA which returns the Regular Expression
-- result for particular patterns.
   where 
      eID = toEventID (expect,pattern)
      pattern = toPattern p
      Expect {regChannel=regChannel} = expect
      register listener = 
         sendIO regChannel (registerPtn pattern eID listener)
      unregister listener = 
         sendIO regChannel (deregisterPtn pattern eID listener)

-- --------------------------------------------------------------------------
--  RST - registration/deregistration and lookup
-- --------------------------------------------------------------------------

emptyRST :: RST 
emptyRST = RST [] emptyFM

-- registerListener eventID listener RST
-- returns new RST as a result of registering listener as interested in
-- eventID
registerListener :: EventDesignator e => e -> Listener -> RST -> IO RST
registerListener eventID listener (RST patterns eventMap) = 
    return (RST patterns (addToFM eventMap eID (listener : listeners)))
  where 
     eID = toEventID eventID
     listeners  = lookupWithDefaultFM eventMap [] eID 

-- deregisterListener eventID listener RST
-- reverses registerListener
deregisterListener :: EventDesignator e => e -> Listener -> RST -> IO RST
deregisterListener eventID listener (RST patterns eventMap) = 
   case filter (/= listener) listeners of
      [] ->  
         return (RST patterns (delFromFM eventMap eID))
      newListeners -> 
         return (RST patterns (addToFM eventMap eID newListeners))
   where  
      eID = toEventID eventID
      listeners = lookupWithDefaultFM eventMap [] eID

-- getListeners looks up a pattern or something else corresponding
-- to an EventDesignator, to see what Listeners to send the result to.
getListeners :: EventDesignator e => e -> RST -> [Listener]
getListeners eventID (RST _ eventMap) = 
   lookupWithDefaultFM eventMap [] (toEventID eventID)


-- registerPtn pattern eventID listener 
-- and
-- deregisterPtn pattern eventID listener
-- are RST -> IO RST registration change things to be sent to
-- along the expect regChannel when a listener is to be added/taken off
-- interested list for a pattern.  So apart from calling 
-- register/deregisterList they must update the pattern list as
-- appropriate.
registerPtn :: Pattern -> EventID -> Listener -> RST -> IO RST
registerPtn pattern eID listener rst = 
   do
      logExpect ("registerPtn" ++ show eID)
      (RST patterns eventMap) <- registerListener eID listener rst
      return (RST (insertOrd higherPriority pattern patterns) eventMap)
   where
      higherPriority (Pattern _ priority1 _) (Pattern _ priority2 _) = 
         priority1 > priority2

deregisterPtn :: Pattern -> EventID -> Listener -> RST -> IO RST
deregisterPtn (Pattern _ _ patStr) eID listener rst = 
   do
      logExpect ("deregisterPtn " ++ show eID)
      (RST patterns eventMap) <- deregisterListener eID listener rst
      let 
         newPatterns = 
            filter 
               (\ (Pattern _ _ patStr') ->
                  patStr' /= patStr
                  )
            patterns
      return (RST newPatterns eventMap)



-- --------------------------------------------------------------------------
--  Logging 
-- --------------------------------------------------------------------------

logMatch :: String -> Pattern -> MatchResult -> IO ()
logMatch buffer pattern@(Pattern _ prio patStr) matchResult = 
   do
      logExpect ("Buffer = " ++ show buffer)
      logExpect ("Pattern = " ++ show prio ++ ":" ++ patStr)
      logExpect ("Matched = " ++ show (getMatched matchResult))
      logExpect ("Rest = " ++ show (getAfter matchResult))

logNoMatch :: String -> IO ()
logNoMatch buffer = logExpect ("NoMatch found in " ++ show buffer)
logAck :: IO()
logAck = 
   do
      logExpect ("Dispatching Done")
      logExpect ("----------------")

logExpect :: String -> IO ()
logExpect buffer = debug ("Expect:"++buffer) 
{-# INLINE logAck #-}
{-# INLINE logMatch #-}
{-# INLINE logExpect #-}



