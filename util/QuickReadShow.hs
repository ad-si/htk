{- QuickReadShow is designed for the rapid manufacture of read/show 
   instances where the instance is created simply by mapping from/to
   another type for which a Read/Show instance already exists 
   For examples see cvs/CVSTypes.hs -}
module QuickReadShow(
   WrapRead(WrapRead), 
   QuickRead(quickRead), -- instance this and you get Read
   WrapShow(WrapShow),
   QuickShow(quickShow) -- instance this and you get Show
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

instance QuickRead toRead => Read toRead where
   readsPrec = mkReadsPrec quickRead

data WrapShow toShow = forall show . Show show => WrapShow (toShow -> show)

class QuickShow toShow where
   quickShow :: WrapShow toShow

mkShowsPrec :: WrapShow toShow -> Int -> toShow -> ShowS
mkShowsPrec (WrapShow convFn) prec value acc =
   showsPrec prec (convFn value) acc 

instance QuickShow toShow => Show toShow where
   showsPrec = mkShowsPrec quickShow
