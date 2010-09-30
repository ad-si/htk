-- | RegexChannel provides Expect-like pattern matching on
-- a channel.  Regular expression matching is done by GNU regex,
-- via RegexString.
--
-- Problems I will admit to:
-- (1) Since GNU regex is effectively a black box, there is no
--     indexing - we must check every regex against every string.
--     This is theoretically bad, because there ought to be
--     some way of constructing a finite-state-automaton which
--     selects the right alternative.
-- (2) Because of the way GuardedChannels work, we can't
--     get out the extra information from the matching
--     (like, the match substrings).  So if we want them,
--     they have to be recomputed.
-- Note the following difference from Einar's Expect:
--    priorities are not available, instead you should put your
--    most important events at the left in +> expressions.
--    If this seems feeble, my excuse is that you had to do this
--    anyway with Einar's expect if you wanted the priorities to
--    work, so the priorities were redundant.
module Events.RegexChannel(
   RegexChannel,  -- channel of Strings
   newRegexChannel, -- :: IO RegexChannel
   sendString, -- :: RegexChannel -> String -> Event ()
   RegularExpression, -- compiled Regular Expression
   HasRegularExpression(..),
      -- class of things which have these,
      -- includes RegularExpression and String
   matchEvent, -- :: HasRegularExpression ptn => ptn -> RegexChannel
      -- -> Event String
   matchAny, -- :: RegexChannel -> Event String
   matchEvent', -- :: HasRegularExpression ptn => ptn -> RegexChannel
      -- -> Event MatchResult
   ) where

import Data.Maybe

import Util.RegularExpression

import Events.Events
import Events.GuardedEvents
import Events.MatchChannel



-- | A channel of Strings, which can be matched by regular expression
newtype RegexChannel = RegexChannel (MatchChannel String)


-- | Create a new RegexChannel
newRegexChannel :: IO RegexChannel
newRegexChannel =
   do
      matchChannel <- newMatchChannel
      return (RegexChannel matchChannel)

-- | Send a String along a RegexChannel.
sendString :: RegexChannel -> String -> Event ()
sendString (RegexChannel matchChannel) str = send matchChannel str

-- | Class of things which may be converted to compiled regular expressions.
-- This includes compiled regular expressions themselves and Strings.
-- This allows the user to speed things up by precompiling regular
-- expressions.
--
-- The syntax of the regular expressions themselves is summarised in the
-- comments to the source file util\/RegularExpression.hs
class HasRegularExpression ptn where
   -- | Compiles the given regular expression.
   toRegularExpression :: ptn -> RegularExpression

instance HasRegularExpression RegularExpression where
   toRegularExpression = id

instance HasRegularExpression String where
   toRegularExpression = compile


-- | Makes Event accepting Strings matching a particular pattern.
matchEvent :: HasRegularExpression ptn => ptn -> RegexChannel -> Event String
matchEvent ptn = matchEventPrim (toRegularExpression ptn)
{-# INLINE matchEvent #-}

-- | Makes Event accepting any String at all.
matchAny :: RegexChannel -> Event String
matchAny = matchEvent ""


-- | Makes Event accepting Strings matching a particular pattern, but
-- carrying the actual MatchResult, allowing for example for
-- parts of the String matching parts of the regular expression to
-- be returned.
matchEvent' :: HasRegularExpression ptn => ptn -> RegexChannel
   -> Event MatchResult
matchEvent' ptn = matchEventPrim' (toRegularExpression ptn)
{-# INLINE matchEvent' #-}

matchEventPrim' :: RegularExpression -> RegexChannel -> Event MatchResult
matchEventPrim' regex channel =
   matchEventPrim regex channel >>>=
      (\ str -> case matchString regex str of
         Just result -> return result)

matchEventPrim :: RegularExpression -> RegexChannel -> Event String
matchEventPrim regex (RegexChannel matchChannel) =
   toEvent (listen matchChannel |>
      Match (\ str -> isJust (matchString regex str)))
