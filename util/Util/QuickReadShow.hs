{-# LANGUAGE ExistentialQuantification #-}

-- | QuickReadShow is designed for the rapid manufacture of read/show
-- instances.  To create such an instance you need to (a) instance
-- quickRead; (b) instance Read/Show using a particular template.
-- (Before April 2004 (b) was not part of the code; it now has to
-- be added to deal with tougher GHC restrictions on overlapping instances.)
module Util.QuickReadShow(
   WrapRead(WrapRead),
   QuickRead(quickRead),
   qRead,

   WrapShow(WrapShow),
   QuickShow(quickShow),
   qShow
   ) where

data WrapRead toRead = forall read . Read read => WrapRead (read -> toRead)

class QuickRead toRead where
   quickRead :: WrapRead toRead

mkReadsPrec :: WrapRead toRead -> Int -> ReadS toRead
mkReadsPrec (WrapRead convFn) prec str =
   let
      parses = readsPrec prec str
   in
      map
         (\ (result,rest) -> (convFn result,rest))
         parses

qRead :: QuickRead toRead => Int -> String -> [(toRead, String)]
qRead = mkReadsPrec quickRead

{- Example instance

instance Read ExampleType where
   readsPrec = qRead
   -}

data WrapShow toShow = forall show . Show show => WrapShow (toShow -> show)

class QuickShow toShow where
   quickShow :: WrapShow toShow

mkShowsPrec :: WrapShow toShow -> Int -> toShow -> ShowS
mkShowsPrec (WrapShow convFn) prec value acc =
   showsPrec prec (convFn value) acc

qShow :: QuickShow toShow => Int -> toShow -> String -> String
qShow = mkShowsPrec quickShow

{- Example instance

instance Show ExampleType where
   showsPrec = qShow
   -}
