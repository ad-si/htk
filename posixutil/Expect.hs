-- | This is a resurrection of the old UniForM Haskell-Expect module,
-- but made rather easier since we have guarded events.
-- 
-- The format of regular expressions is described in 
-- "RegularExpression".
-- 
-- There is one important difference between this Expect implementation
-- and Einar's, namely that we don't allow priorities.  Instead, you
-- must state higher-priority events first in choice operators such as
-- +>.  As a matter of fact you had to do this with Einar's implementation
-- anyway, so this is not really a restriction.
module Expect(
   Expect, -- a running tool with Expect.
   newExpect, -- :: FilePath -> [Config PosixProcess] -> IO Expect
      -- start one up

   -- Here's how you read the results.  Note that results are
   -- fed out one by one, so you don't get to see line N+1 until
   -- you've handled line N.
   expect, -- :: HasRegularExpression ptn => Expect -> ptn -> Event String
      -- match returning the whole String
   match, -- :: HasRegularExpression ptn => Expect -> ptn -> Event MatchResult
      -- match returning a MatchResult, allowing you to get out matched
      -- substrings and other information.
   matchEOF, -- :: Expect -> Event ()
      -- used on EOF.
   matchLine, -- :: Expect -> Event String
      -- matches any line.
   ) where

import Control.Concurrent

import Object
import IOExtras
import Computation
import RegularExpression

import Events
import Channels
import Spawn
import RegexChannel
import Destructible

import ChildProcess
import ProcessClasses


-- --------------------------------------------------------------------------
-- The Expect type and its instances
-- --------------------------------------------------------------------------

data Expect =
   Expect {
      child :: ChildProcess, -- the tool
      childOutput :: RegexChannel, -- the output
      eofChannel :: Channel (), -- eof signal
      destroyAction :: IO () -- action to be done by "destroy" instance
      }

instance Tool Expect where
   getToolStatus expect = getToolStatus (child expect)

instance Object Expect where
   objectID expect = objectID (child expect)

instance Destroyable Expect where
   destroy expect = destroyAction expect

instance CommandTool Expect where
   execOneWayCmd str expect = sendMsg (child expect) str
   execCmd = execOneWayCmd
   evalCmd str expect = error "Expect tools do not permit evalCmd"
      -- because you have to get the output out with the pattern matchers.

-- --------------------------------------------------------------------------
-- Starting Expect
-- --------------------------------------------------------------------------

newExpect :: FilePath -> [Config PosixProcess] -> IO Expect
newExpect filePath options =
   do
      child <- newChildProcess filePath options

      childOutput <- newRegexChannel
      eofChannel <- newChannel
      destroyMVar <- newMVar (destroy child)
         -- We don't destroy the reader thread, because
         -- (a) we'd need a special channel to do it;
         -- (b) it will die eventually anyway when it receives an
         --     EOF, or when we stop listening to it.
      let    
         destroyAction =
            do
               action <- takeMVar destroyMVar
               putMVar destroyMVar done
               action
         expect = Expect {
            child = child,
            childOutput = childOutput,
            eofChannel = eofChannel,
            destroyAction = destroyAction
            }
  
      spawn (reader expect)
      
      return expect

-- --------------------------------------------------------------------------
-- The reader thread
-- --------------------------------------------------------------------------

reader :: Expect -> IO ()
reader (expect@Expect {child = child,childOutput = childOutput,
      eofChannel = eofChannel}) =
   do
      nextLineOpt <- catchEOF(readMsg child)
      -- An exception here (should) mean some non-EOF file error.
      case nextLineOpt of
         Nothing -> sendIO eofChannel ()
         Just nextLine -> 
            do
               sync(sendString childOutput nextLine)
               reader expect

-- --------------------------------------------------------------------------
-- Reading the output.
-- We inline functions for speed, in particular so that constant strings get 
-- converted to constant compiled regular expressions.
-- --------------------------------------------------------------------------

expect :: HasRegularExpression ptn => Expect -> ptn -> Event String
expect (Expect {childOutput = childOutput}) ptn = matchEvent ptn childOutput
{-# INLINE expect #-}

match :: HasRegularExpression ptn => Expect -> ptn -> Event MatchResult
match (Expect {childOutput = childOutput}) ptn = matchEvent' ptn childOutput
{-# INLINE match #-}

matchEOF :: Expect -> Event ()
matchEOF (Expect {eofChannel = eofChannel}) = receive eofChannel
{-# INLINE matchEOF #-}

matchLine :: Expect -> Event String
matchLine (Expect {childOutput = childOutput}) = matchAny childOutput
{-# INLINE matchLine #-}
     