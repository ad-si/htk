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
DESCRIPTION   : (Einar's commments are now somewhat out of date.  See below
                for changes.)


                Provides similar functionality as the expect tool documented 
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

Changes:
   1) use of the GHC RegexString interface instead of the above.
      (This eventually calls the GNU regexp library, so should be
      much quicker.)
   2) The reader thread and writer thread now communicate by a 
      channel.  The child process is run in line mode (normally) and
      the writer thread gets the lines complete. 
   3) The registration channel is now made a MsgQueue (so if you
      send something on it you don't have to wait for something t
      pick it up) and attemptMatch is the only function which looks
      at it.  This simplifies the code somewhat.

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
   matchLongLine, -- do not use !
   readWholeLine,
   matchLine,
   
   commandFailed
   ) where

#ifdef DEBUG
import qualified IOExts(unsafePerformIO)
#endif

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
      childOutput     :: Channel String,
      -- the reader sends each line output by the child along this
      -- channel.
      eofChannel      :: Channel (),           -- eof signal  
      -- the reader sends a message along this channel when it
      -- gets an error.
      regChannel      :: MsgQueue (RST -> IO RST), 
      -- Registration changes.  A registration change is a function
      -- from RST to IO RST which you send along regChannel and which
      -- is read by the matcher thread.  The function had better be
      -- quickly handled.
      -- Why a queue and not a channel?  Because we terminate the
      -- matcher thread when EOF is received, so there is no-one
      -- at home to receive any final registration changes.
      collectable     :: Maybe CollectibleObj
      }

data Pattern = Pattern RegularExpression Int String  
-- compiled pattern, priority, original string

data EOF = EOF

data RST = RST [Pattern] (FiniteMap EventID [Listener])
-- RST contains the state for the matcher.
-- RST contains the map from Patterns to channels, arranged in two ways.
-- The list contains all patterns matched.    The FiniteMap indexes
-- them (by EventID), giving the list of listeners to send to, in
-- order of priority.
-- (The FiniteMap can also contain the EOF event.)


-- --------------------------------------------------------------------------
--  Expect Tool
-- --------------------------------------------------------------------------

newExpect :: FilePath -> [Config PosixProcess] -> IO Expect
newExpect tool confs = 
   do 
      childOutput <- newChannel
      eofChannel <- newChannel
      regChannel <- newMsgQueue
      child <- newChildProcess tool ((linemode True) : confs)
         -- this means the child process runs with linemode True
         -- UNLESS the configurations say otherwise.  However that
         -- is not recommended.
      let 
         expect =
            Expect {
               child=child,
               childOutput=childOutput,
               eofChannel=eofChannel,
               regChannel=regChannel,
               collectable=Nothing
               }
      forkIO (matcher expect emptyRST)
      forkIO (reader expect)
      cobj <- newCollectibleObj
      -- specify that when we want to GC this Expect instance
      -- we destroy the childprocess
      destructor (destroy child) cobj
      return (expect {collectable = Just cobj}) 

-- --------------------------------------------------------------------------
--  Reader Thread.  This reads messages from the child and passes them
--  along the childOutput channel.
-- --------------------------------------------------------------------------

reader :: Expect -> IO ()
reader expect@
      (Expect{child=child,childOutput=childOutput,eofChannel=eofChannel}) = 
   do
      ans <- try(readMsg child)
      case ans of
         Left e  ->  
            do -- I think this means the file was closed
               sendIO eofChannel ()         -- EOF
               closeChildProcessFds child
         Right v ->  
            do
               sendIO childOutput v
               reader expect

-- --------------------------------------------------------------------------
--  Dispatcher and Adaptor Thread
-- --------------------------------------------------------------------------

matcher :: Expect -> RST -> IO ()
matcher 
      expect@(Expect{
         childOutput=childOutput,
         eofChannel=eofChannel,
         regChannel=regChannel})
      rst = 
   sync (
         receive childOutput >>>=
            (\line ->
               -- message received from the tool via "reader".
               do
                  attemptMatch expect line rst
               )
      +> receive eofChannel >>> 
           do
              delegateEOF expect rst
      )

-- attemptMatch finds and handles all the non-overlapping matched
-- patterns in a line, then returns the new registration
-- event.
attemptMatch :: Expect -> String -> RST -> IO ()
attemptMatch expect line oldRst = 
   do
      rst@(RST patterns _) <- 
         getPendingChanges (regChannel expect) oldRst
      case (matchPattern patterns line) of 
         Nothing -> 
            -- wait for matching patterns to arrive
            do
               debug "attemptMatch 1"
               newRst <-  getOneChange (regChannel expect) rst
               debug "attemptMatch 2"
               attemptMatch expect line newRst
         Just (pattern,matchResult) -> 
            do
               logMatch line pattern matchResult
               delegateEvent expect pattern matchResult rst

-- matchPattern patterns str 
-- searches for the first matching pattern in the list of patterns.
-- It returns the corresponding pattern,
-- plus the MatchResult value.
matchPattern :: [Pattern] -> String -> Maybe (Pattern,MatchResult)
matchPattern patterns string =
   firstJust (map (matchOnePattern string) patterns)

matchOnePattern :: String -> Pattern -> Maybe (Pattern,MatchResult)
matchOnePattern string pattern @ (Pattern regularExpression _ patString) = 
#ifdef DEBUG
   IOExts.unsafePerformIO(
      do
         debug("Is "++patString++" in "++string)
         case matchString regularExpression string of
            Nothing -> 
               do
                  debug "No"
                  return Nothing
            Just matchResult ->
               do 
                  debug matchResult
                  return (Just(pattern,matchResult))
      )
 
#else
   case matchString regularExpression string of
      Nothing -> Nothing
      Just matchResult -> Just (pattern,matchResult)
#endif
{- delegateEvent handles a successful match.
   delegateEvent expect pattern matchResult est
   pattern is the successful match.
   -}
delegateEvent :: Expect -> Pattern -> MatchResult -> RST -> IO ()
delegateEvent 
      expect @ (Expect{regChannel=regChannel}) pattern matchResult rst =
   do
      nextLineQueue <- newMsgQueue
      let
         toWaitFor = receive nextLineQueue
         toSend bool = send nextLineQueue bool
      nextLine <- sync (
         choose (map
            (\ listener -> oneway listener eID (matchResult,toSend)) 
            listeners)
            >>>= -- wait until one of the listeners accepts the event.
               (\ ev ->
                   -- this is the event to wait for for an acknowledgment.
                  do
                     sync ev
                     nextLine <- sync toWaitFor
                     logAck
                     return nextLine
                  )
         )
      if nextLine
         then
            matcher expect rst
         else
            attemptMatch expect restLine rst              
  where 
     eID = toEventID (expect,pattern)
     listeners = getListeners eID rst
     restLine = getAfter matchResult

delegateEOF :: Expect -> RST -> IO ()
-- delegateEOF is similar to delegateEvent but is called when an
-- EOF message is received from the reader.  It also has to get
-- pending registration changes (which delegateEvent doesn't have
-- to do as attemptMatch has just done it.
delegateEOF expect @ (Expect{regChannel=regChannel}) oldRst =
   do 
      rst <- getPendingChanges regChannel oldRst
      let 
         eID = toEventID (expect,EOF)
         listeners = getListeners eID rst
      sync (  
         choose (map (\ listener -> oneway listener eID ()) listeners) >>>= 
            (\ ev -> -- this is the event to wait for for an acknowledgment. 
               do
                  sync ev
                  logAck
               )
         )
      done

-- --------------------------------------------------------------------------
-- Actions which receive and process registration events
-- --------------------------------------------------------------------------

-- getOneChange waits for one change
getOneChange :: MsgQueue (RST -> IO RST) -> RST -> IO RST
getOneChange regChannel rst = sync (registrationChanged regChannel rst)

registrationChanged :: MsgQueue (RST -> IO RST) -> RST -> EV RST
registrationChanged regChannel rst = 
   receive regChannel >>>= 
      (\ registrationChange -> registrationChange rst )


-- getPendingChanges gets all pending changes.
getPendingChanges :: MsgQueue (RST -> IO RST) -> RST -> IO RST 
getPendingChanges regChannel rst =
   do 
      optionalChange <- poll(receive regChannel)
      case optionalChange of
         Nothing -> return rst
         Just registrationChange ->
            do
               newRst <- registrationChange rst 
               getPendingChanges regChannel newRst 


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

expect :: PatternDesignator ptn => Expect -> ptn -> IA String
expect exp ptn = 
   match exp ptn >>>=
      ( \ matchResult -> return (getMatched matchResult))
 

expect' :: PatternDesignator ptn => Expect -> ptn -> 
   IA (String,Bool -> EV ())
expect' exp ptn = 
   match' exp ptn >>>= 
      ( \ (matchResult,nextLine) ->
         return (getMatched matchResult,nextLine)
         )


readWholeLine :: Expect -> (String -> IO String) -> IO String
-- readWholeLine should only be used when Expect is being used
-- in non-line mode and we need a complete line from it
-- (which may mean taking several blocks and joining them
-- together.)  The second argument is an action to
-- call (with the string so far) if EOF occurs.
readWholeLine expect eofAct = readWholeLineAcc [] 
   where
      readWholeLineAcc :: [String] -> IO String
      -- the accumulating parameter contains the strings
      -- so far read, in reverse order.
      readWholeLineAcc soFar =
         sync(
               matchWithNewLine >>>=
                  ( \ line ->
                     do
                        let result = concat(reverse(line:soFar))
                        debug result
                        return result
                     )                     
            +> matchWithoutNewLine >>>=
                  ( \ line -> 
                     readWholeLineAcc (line:soFar)
                     )
            +> matchEOF expect >>> eofAct (concat(reverse soFar))
            )

      matchWithNewLine :: IA String 
         -- match newline and don't advance
      matchWithNewLine  =
         match' expect "\\`(.*)\n" >>>=
            ( \ (matchResult,nextLine) ->
               do
                  sync(nextLine False)
                  let head : _ = getSubStrings matchResult
                  return head
               )
      matchWithoutNewLine :: IA String 
         -- match whole of remaining buffer if without newline, and do
         -- advance
      matchWithoutNewLine =
         match' expect "\\`.*\\'" >>>=
            ( \ (matchResult,nextLine) ->
               do
                  sync(nextLine True)
                  return(getMatched matchResult)
               )

matchLongLine = error "Expect.matchLongLine used"
{- 
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
-}             

matchLine:: Expect -> IA String
matchLine exp = expect exp "^.*$" 


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
      deregister listener = 
         sendIO regChannel (deregisterListener eID listener)


-- --------------------------------------------------------------------------
--  Matching Event 
-- --------------------------------------------------------------------------

match :: PatternDesignator ptn => Expect -> ptn -> IA MatchResult 
match expect ptn  =
   match' expect ptn >>>=
      (\ (matchResult,nextLine) ->
         do
            sync(nextLine True)
            return matchResult
         )

match' :: PatternDesignator ptn => Expect -> ptn -> 
   IA (MatchResult,Bool -> EV()) 
match' expect ptn = interaction eID register unregister 
-- Use match to make an IA which returns the Regular Expression
-- result for particular patterns.
   where 
      eID = toEventID (expect,pattern)
      pattern = toPattern ptn
      Expect {regChannel=regChannel} = expect
      register listener = 
         do
            debug "register1"
            sendIO regChannel (registerPtn pattern eID listener)
            debug "register2"
      unregister listener = 
         do
            debug "unregister1"
            sendIO regChannel (deregisterPtn pattern eID listener)
            debug "unregister2"

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
logMatch line pattern@(Pattern _ prio patStr) matchResult = 
   do
      logExpect ("Line = " ++ show line)
      logExpect ("Pattern = " ++ show prio ++ ":" ++ patStr)
      logExpect ("Matched = " ++ show (getMatched matchResult))
      logExpect ("Rest = " ++ show (getAfter matchResult))

logNoMatch :: String -> IO ()
logNoMatch line = logExpect ("NoMatch found in " ++ show line)
logAck :: IO()
logAck = 
   do
      logExpect ("Dispatching Done")
      logExpect ("----------------")

logExpect :: String -> IO ()
logExpect mess = debug ("Expect:"++mess) 
{-# INLINE logAck #-}
{-# INLINE logMatch #-}
{-# INLINE logExpect #-}



