-- | The LineShow type is simply a list type except that it has
-- Read and Show instances which put the output line by line,
-- preceded by the number of lines.  This is useful for data
-- files stored by CVS and similar systems.
module Util.LineShow(
   LineShow(..)
   ) where

newtype LineShow a = LineShow [a]

instance Show a => Show (LineShow a) where
   showsPrec prec (LineShow list) acc =
      let
         showLines [] acc = acc
         showLines (h:t) acc = showLines t (showsPrec prec h ('\n':acc))
      in
         (show (length list))++('\n':showLines list acc)

instance Read a => Read (LineShow a) where
   readsPrec prec toRead =
      let
         readLines 0 acc toRead = [(LineShow acc,toRead)]
         readLines nLeft acc toRead =
            case readsPrec prec toRead of
               [(this,'\n':remainder)] ->
                  readLines (nLeft-1) (this:acc) remainder
               _ -> []
      in
         case readsPrec prec toRead of
            [(nLines,'\n':remainder)] -> readLines (nLines :: Int) [] remainder
            _ -> []

