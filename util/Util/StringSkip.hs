-- | This module implements the StringSkip monad, which is a monad for
-- going through a String from the start to the end doing various
-- insertions\/deletions along the way.
module Util.StringSkip (
   StringSkip, -- the monad

   runStringSkip,
      -- :: StringSkip a -> String -> (a,String)
      -- transform a String with the given StringSkip.

   atEnd,
      -- :: StringSkip Bool
      -- point is at end of String

   copy,
      -- :: StringSkip Char
      -- copy the next character and point after it.


   copyOrEnd,
      -- :: StringSkip (Maybe Char)
      -- copy the next character, or return Nothing if at end.

   skip,
      -- :: StringSkip Char
      -- skip the next character (so don't copy it and point after) and
      -- return it.

   skipIf,
      -- :: (Char -> Bool) -> StringSkip (Maybe Char)
      -- If the character matches the given function, skip it, and return it.

   copyAfter,
      -- :: String -> StringSkip ()
      -- Copy up to the given String, and over it.

   insertBefore,
      -- :: String -> StringSkip ()
      -- Insert given string before point.

   matchCopy,
      -- :: String -> StringSkip Bool
      -- If the argument is a prefix of the remaining string, copy past and
      --   return True.

   match,
      -- :: String -> StringSkip Bool
      -- If the argument is a prefix of the remaining String, return True
      -- (but don't change anything).
   ) where

import List
import Maybe

import Util.ExtendedPrelude

-- -------------------------------------------------------------------------
-- The types
-- -------------------------------------------------------------------------

data State = State {
   bp :: String, -- ^ stuff before the point, in reverse order
   ap :: String -- ^ stuff after the point
   }


newtype StringSkip a = StringSkip (State -> (a,State))

-- --------------------------------------------------------------------------
-- The Monad instance
-- --------------------------------------------------------------------------


instance Monad StringSkip where
   return a = StringSkip (\ state -> (a,state))
   (>>=) (StringSkip f1) get2 =
      StringSkip (\ state0 ->
         let
            (a,state1) = f1 state0
            (StringSkip f2) = get2 a
         in
            f2 state1
         )

-- --------------------------------------------------------------------------
-- Running stringSkip
-- --------------------------------------------------------------------------

runStringSkip :: StringSkip a -> String -> (a,String)
runStringSkip (StringSkip f) s =
   let
      state0 = State {bp = [],ap = s}
      (a,state1) = f state0
   in
      (a,revAppend (bp state1) (ap state1))

-- --------------------------------------------------------------------------
-- StringSkip values
-- --------------------------------------------------------------------------

atEnd :: StringSkip Bool
atEnd = StringSkip (\ state -> (null (ap state),state))


copy :: StringSkip Char
copy = copy1 "Copy at end"

copy1 :: String -> StringSkip Char
copy1 mess = StringSkip (\ (State {bp = bp0,ap = ap0}) ->
   case ap0 of
      c : ap1 -> (c,State {bp = c : bp0,ap = ap1})
      [] -> error mess
      )

copyOrEnd :: StringSkip (Maybe Char)
copyOrEnd = StringSkip (\ (state @ State {bp = bp0,ap = ap0}) ->
   case ap0 of
      c : ap1 -> (Just c,State {bp = c : bp0,ap = ap1})
      [] -> (Nothing,state)
      )

skip :: StringSkip Char
skip = StringSkip (\ (State {bp = bp0,ap = ap0}) ->
   case ap0 of
      c : ap1 -> (c,State {bp = bp0,ap = ap1})
      [] -> error "Skip at end"
      )

skipIf :: (Char -> Bool) -> StringSkip (Maybe Char)
skipIf f = StringSkip (\ (state @ (State {bp = bp0,ap = ap0})) ->
   case ap0 of
      c : ap1
         | f c
         -> (Just c,State {bp = bp0,ap = ap1})
      _ -> (Nothing,state)
      )


insertBefore :: String -> StringSkip ()
insertBefore str = StringSkip (\ (State {bp = bp0,ap = ap0}) ->
   let
      bp1 = revAppend str bp0
   in
      (((),State {bp = bp1,ap = ap0}))
   )

matchCopy :: String -> StringSkip Bool
matchCopy str = StringSkip (\ (state @ (State {bp = bp0,ap = ap0})) ->
   case isPrefix str ap0 of
      Nothing -> (False,state)
      Just ap1 -> (True,State {bp = revAppend str bp0,ap = ap1})
   )


match :: String -> StringSkip Bool
match str = StringSkip (\ (state @ (State {ap = ap0})) ->
   (isJust (isPrefix str ap0),state)
   )


copyAfter :: String -> StringSkip ()
copyAfter str =
   do
      b <- matchCopy str
      if b
         then
            return ()
         else
            do
               copy1 ("Not found " ++ str)
               copyAfter str

-- --------------------------------------------------------------------------
-- Elementary functions I can't find in the standard libraries
-- --------------------------------------------------------------------------

revAppend :: [a] -> [a] -> [a]
revAppend [] as = as
revAppend (a : as1) as2 = revAppend as1 (a : as2)
