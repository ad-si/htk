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
        ToolStatus(..),

        Destructible(..),
        Tool(..),
        UnixTool(..),
        CommandTool(..),
 
        Expect,
        newExpect,

        MatcherFlag(..),

        -- RegexpResponse is the type of the result of a successful regular 
        -- expression search.  wholeMatch,beforeMatch,afterMatch
        -- take it apart, returning the parts of the string in the
        -- pattern, before the pattern, and after the pattern.
        RegexpResponse,
        
        wholeMatch, -- :: RegexpResponse -> String
        beforeMatch, -- :: RegexpResponse -> String
        afterMatch, -- :: RegexpResponse -> String

        PatternDesignator(..),

        match,
        expect,
        matchEOF,
        matchLongLine,
        matchLine,

        commandFailed
        ) where

import Concurrency
import Interaction
import Listener

import FiniteMap
import ChildProcess
import Interpreter(commandFailed)
import SIMClasses
import Object
import Collector
import Dynamics
import Posix(signalProcess,sigKILL)

import ExtendedPrelude(insertOrd)
import Regex
import PackedString
import Debug(debug)

import qualified IOExts(unsafePerformIO)

-- --------------------------------------------------------------------------
-- Matcher Flags.
-- We have to use IOExts.unsafePerformIO in these functions.  In both
-- cases it is because we are wrapping an "obviously' pure function
-- that is however an action.
-- --------------------------------------------------------------------------
-- This are inherited from the Regexp library which UniForM used to
-- use.  The GHC/GNU regexp library instead uses characters.
data MatcherFlag = 
      Case_Insensitive -- ignore case when using strings
   |  Case_Sensitive   -- don't ignore case when using strings 
   |  Single_Line      -- treat string as a single line
                       -- . matches \n
   |  Multi_Line       -- treat string as multi-line 
                       -- (^ ($) matches from beginning (end) of line)
   deriving (Read,Show,Eq)

-- other options were provided by the Regexp library but are not
-- used anywhere in UniForM.  In fact the only options UniForM seems
-- to use are Case_Insensitive and Multi_Line.

-- decodeFlags turns a list of MatchFlags into a pair of bools
-- equivalent to those currently demanded by Regex.re_compile_pattern,
-- so the first is true iff single-line mode; the second is true iff
-- case-insensitive.  The default is single line mode and case sensitive.
-- Where there are contradictory flags in the list, the first flag wins.
decodeFlags :: [MatcherFlag] -> (Bool,Bool)
decodeFlags flags = 
   (singleLine flags,insensitive flags)
      where
         singleLine [] = True
         singleLine (Single_Line : _) = True
         singleLine (Multi_Line : _) = False
         singleLine (_ : rest) = singleLine rest
         insensitive [] = False
         insensitive (Case_Insensitive : _) = True
         insensitive (Case_Sensitive : _) = False
         insensitive (_ : rest) = insensitive rest

makePattern :: String -> [MatcherFlag] -> PatBuffer
makePattern stringPattern flags  =
   let 
      (singleline,insensitive) = decodeFlags flags
      packedStringPattern = packString stringPattern
   in
      IOExts.unsafePerformIO(
         re_compile_pattern packedStringPattern singleline insensitive
         )

-- pure_re_search is a unsafely wrapped version of re_search.
-- This certainly ought to be safe, since re_search appears to
-- be a pure function.  For documentation of the arguments see
-- the documentation for re_search
pure_re_search :: PatBuffer -> PackedString -> Int -> Int -> Bool -> 
   Maybe REmatch
pure_re_search pattern target start stop record= 
   IOExts.unsafePerformIO(re_search pattern target start stop record)

-- Here is the type of the response from matchPattern; it contains
-- the search results as a Regex.REmatch,and the original string 
type RegexpResponse = (REmatch,PackedString)

getPart :: PackedString -> (Int,Int) -> String
getPart ps (start,end) = unpackPS(substrPS ps start (end-1))

wholeMatch :: RegexpResponse -> String
wholeMatch(REmatch arr before whole after lastbr,ps) = getPart ps whole

beforeMatch :: RegexpResponse -> String
beforeMatch(REmatch arr before whole after lastbr,ps) = getPart ps before

afterMatch :: RegexpResponse -> String
afterMatch(REmatch arr before whole after lastbr,ps) = getPart ps after


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Expect = 
        Expect {
                dispatcher      :: ChildProcess,         -- unix server
                eventsap        :: SAP String (),        -- new messages
                eofchannel      :: Channel (),           -- eof signal  
                regchannel      :: Channel (RST -> IO RST), 
                                                         -- new registration
                collectable     :: Maybe CollectibleObj
                }

type Pattern = (PatBuffer,Int,String)  
-- compiled pattern, priority, original string
-- The default priority is 0.  In a contest between two patterns,
-- the one with the highest priority wins.

data EOF = EOF

type RST = ([Pattern],FiniteMap EventID [Listener])

type EST = (String,RST)


-- --------------------------------------------------------------------------
--  Expect Tool
-- --------------------------------------------------------------------------

newExpect :: FilePath -> [Config PosixProcess] -> IO Expect
newExpect tool confs = do {
        sap <- newSAP;
        cha <- newChannel;
        chr <- newChannel;
        disp <- newChildProcess tool confs';
        exp <- return (Expect disp sap cha chr Nothing);
        forkIO (matcher exp ("",([],emptyFM)));
        forkIO (reader exp);
        cobj <- newCollectibleObj;
        cobj # destructor (destroy disp);
        return (Expect disp sap cha chr (Just cobj)) 
} where confs' = (linemode False):confs
        

-- --------------------------------------------------------------------------
-- Class Pattern Designators
-- --------------------------------------------------------------------------

class PatternDesignator p where
        toPattern :: p -> Pattern       

instance PatternDesignator [Char] where
        toPattern str = (makePattern str [],0,str)

instance PatternDesignator ([Char],Int) where
        toPattern (str,prio) = (makePattern str [],prio,str)

instance PatternDesignator ([Char],Int,[MatcherFlag]) where
        toPattern (str,prio,flags) = (makePattern str flags,prio,str)

instance PatternDesignator ([Char],[MatcherFlag]) where
        toPattern (str,flags) = (makePattern str flags,0,str)

                                
-- --------------------------------------------------------------------------
--  Instance Typeable and EventDesignator
-- --------------------------------------------------------------------------

expectResponseT :: TyCon
expectResponseT = mkTyCon "Expect" "RegexpResponse"

rEMatchTag = mkTypeTag (mkTyCon "Regex" "REmatch") []
instance Typeable REmatch where
   typeOf rm = rEMatchTag

packedStringTag = mkTypeTag (mkTyCon "PackedString" "PackedString") []
instance Typeable packedString where
   typeOf rm = packedStringTag

instance EventDesignator (Expect,String) where
        toEventID (exp,ptn) = EventID (objectID exp) ('P':ptn)


instance EventDesignator (Expect,EOF) where
        toEventID (exp,ptn) = EventID (objectID exp) ("EOF")


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Expect where
        objectID exp = objectID (dispatcher exp)

instance Destructible Expect where
        destroy exp     = destroy (dispatcher exp)
        destroyed exp   = destroyed (dispatcher exp)    

instance Collectible Expect where
        getCollectibleObj exp = case collectable exp of {Just co -> co}

instance Tool Expect where
        getToolStatus exp = getToolStatus (dispatcher exp)


instance UnixTool Expect where
        getUnixProcessID exp = getUnixProcessID (dispatcher exp)  
        

instance CommandTool Expect where 
        execCmd cmd exp = execOneWayCmd cmd exp
        evalCmd cmd exp = raise commandFailed       
        execOneWayCmd cmd exp  = sendMsg (dispatcher exp) cmd 



-- --------------------------------------------------------------------------
--  Derived Events
-- --------------------------------------------------------------------------

expect :: PatternDesignator p => Expect -> p -> IA String
expect exp ptn = match exp ptn >>>= return . wholeMatch


matchLongLine:: Expect -> IA String
matchLongLine exp = expect exp "^.*" >>>= sync . (matchTail exp)
 where  matchTail :: Expect -> String -> IA String
        matchTail exp l =
                expect exp ".*" >>>= sync.(matchTail exp).(l++)
          +>    expect exp "\n" >>> return (l++"\n")


matchLine:: Expect -> IA String
matchLine exp = expect exp "^.*\n" 


-- --------------------------------------------------------------------------
--  EOF Event 
-- --------------------------------------------------------------------------

matchEOF :: Expect -> IA ()
matchEOF exp = (interaction ev register deregister :: IA ()) >>> do {
        try(getToolStatus exp); 
        done
        }
 where  ev              = toEventID (exp,EOF)
        chr             = regchannel $! exp
        register iact   = sendIO chr (registerListener ev iact) 
        deregister iact = sendIO chr (deregisterListener ev iact) 


-- --------------------------------------------------------------------------
--  Matching Event 
-- --------------------------------------------------------------------------

match :: PatternDesignator p => Expect -> p -> IA RegexpResponse 
match exp p = interaction ev register unregister 
   where ev = toEventID (exp,ptnStr)
         pattern@(_,_,ptnStr) = toPattern p
         chr = regchannel $! exp
         register iact = sendIO chr (registerPtn pattern ev iact)
         unregister iact = sendIO chr (deregisterPtn pattern ev iact)

         registerPtn pattern ev iact rst = do
                debug ("Expect Register " ++ show ev)
                (ptns,rs) <- registerListener ev iact rst
                return (insertOrd higherPriority pattern ptns,rs)
         
         higherPriority (_,p1,_) (_,p2,_) = p1 > p2

         deregisterPtn (_,_,ptnStr) ev iact rst = do
            debug ("Expect Deregister " ++ show ev)
            (ptns,rs) <- deregisterListener ev iact rst
            return (filter (\(_,_,ptnStr') -> ptnStr' /= ptnStr) ptns,rs)



-- --------------------------------------------------------------------------
--  Reader Thread 
-- --------------------------------------------------------------------------

reader :: Expect -> IO ()
reader exp@(Expect disp sap cha _ _) = do {
        ans <- try(readMsg disp);
        case ans of
                Left e  ->  do {
                        sendIO cha ();          -- EOF
                        closeChildProcessFds disp               
                        }
                Right v ->  do {
                        sync(call sap v); 
                        reader exp
                        }
}
        
                        
-- --------------------------------------------------------------------------
--  Dispatcher and Adaptor Thread
-- --------------------------------------------------------------------------

matcher :: Expect -> EST -> IO ()
matcher exp @ (Expect _ sap cha chr _) state@(buf,est) = sync (
        provide sap (\msg -> do {
                        state' <- attemptMatch exp (buf ++ msg,est);
                        return ((),state')
                        }) >>>= matcher exp
  +>    registrationChanged chr state >>>=  (\state' -> do {
                state'' <- attemptMatch exp state';
                matcher exp state''
                })
  +>    whenEV (buf == "") (receive cha >>> do {
                state' <- delegateEOF exp state;
                matcher exp state'
                })              
  ) 


attemptMatch :: Expect -> EST -> IO EST 
attemptMatch exp ("",est) = return ("",est)
attemptMatch exp state@(buf,(ptns,_)) = do {
        case (matchPattern ptns buf) of 
           Nothing -> do {
                logMatch buf "" buf;            
                return state;
                }
           Just (ptn,rem) -> do {
                logMatch buf ptn (afterMatch rem);
                delegateEvent exp ptn rem state
                }                               
} 

delegateEvent :: Expect -> String -> RegexpResponse -> EST -> IO EST
delegateEvent exp @ (Expect _ _ _ chr _) ptn rem state@(buf,est)= sync (
        choose (map (\iact -> oneway iact eid rem) lst) >>>= (\ev -> do {
                (_,est') <- awaitReply' chr ev state;
                logAck;
                attemptMatch exp (afterMatch rem,est')
                })
  +>    registrationChanged chr state >>>= 
                attemptMatch exp
 )
  where eid = toEventID (exp,ptn)
        lst = getListeners eid est


delegateEOF :: Expect -> EST -> IO EST
delegateEOF exp @ (Expect _ _ _ chr _) state@(_,est)= sync (  
        choose (map (\iact -> oneway iact eid ()) lst) >>>= (\ev -> 
                awaitReply' chr ev state)
   +>   registrationChanged chr state
 )
        where   eid = toEventID (exp,EOF)
                lst = getListeners eid est



awaitReply' :: Channel (RST -> IO RST) -> EV () -> EST -> IO EST
awaitReply' chr reply state = sync (
        reply                         >>> return state 
     +> registrationChanged chr state >>>= awaitReply' chr reply
     )


registrationChanged :: Channel (RST -> IO RST) -> EST -> EV EST
registrationChanged chr (buf,rst) = receive chr >>>= \f -> do {
        rst' <- f rst;
        return (buf,rst')
        }


-- --------------------------------------------------------------------------
--  Pattern Matching 
-- --------------------------------------------------------------------------

matchPattern :: [Pattern] -> String -> Maybe (String,RegexpResponse)
matchPattern [] str = Nothing
matchPattern ((ptn,_,patStr) : ptns) str =
        case searchRes of
           Nothing -> matchPattern ptns str
           Just contents -> Just(patStr,(contents,packedString))
        where
           packedString = packString str
           length = lengthPS packedString
           searchRes = pure_re_search ptn packedString 0 length True 

-- --------------------------------------------------------------------------
--  Logging 
-- --------------------------------------------------------------------------

logMatch buf ptn newbuf = do {
        debug ("Buffer = " ++ show buf);
        debug ("Pattern = " ++ show ptn);
        debug ("Rest = " ++ show newbuf);
}
        
logAck = do {
        debug ("Dispatching Done");
        debug ("----------------");
        }



-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

registerListener :: EventDesignator e => e -> Listener -> RST -> IO RST
registerListener ev iact (pts,rs) = return (pts,addToFM rs eid (iact : lst))
  where eid = toEventID ev
        lst = lookupWithDefaultFM rs [] eid 


deregisterListener :: EventDesignator e => e -> Listener -> RST -> IO RST
deregisterListener ev iact (pts,rs) = 
        case filter (/= iact) lst of
                [] ->   return (pts,delFromFM rs eid)
                lst' -> return (pts,addToFM rs eid lst')
 where  eid = toEventID ev
        lst = lookupWithDefaultFM rs [] eid


getListeners :: EventDesignator e => e -> RST -> [Listener]
getListeners ev (pts,rs) = lookupWithDefaultFM rs [] (toEventID ev)

