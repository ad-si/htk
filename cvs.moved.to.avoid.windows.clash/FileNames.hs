{- FileNames contain facilities for manipulating filenames
   in a hopefully OS-independent manner.  At the moment it
   assumes Unix -}
module FileNames(
   fileSep, -- :: Char
            -- file separator
   trimDir -- :: String -> String
            -- trim file separator from end of name if there.
            -- (intended for directories)
   ) where

fileSep :: Char
fileSep = '/'

trimDir :: String -> String
trimDir [] = []
trimDir (name@[c]) 
   | c==fileSep = []
   | True = name
trimDir (first:rest) = first:trimDir rest

