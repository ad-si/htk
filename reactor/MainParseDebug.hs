{- This is a Main program,  which finds the commands sent
   to child processes compiled with Debug mode from the
   /tmp/uniform.DEBUG file.  (The only problem at the
   moment is that can't distinguish between different
   child processes so the commands are all muddled up.
   -}
module Main(main) where

import System.IO
import Util.WBFiles
import qualified Control.Exception as Exception

output :: Handle -> IO ()
output handle =
   do
      line' <- Exception.try(hGetLine handle)
      case line' of
         Right ('\"':'C':'h':'i':'l':'d':'P':'r':'o':'c':'e':'s':
            's':'>':rest) ->
            do
               putStr   (read ('\"':rest))
               output handle
         Right _ -> output handle
         Left e  -> const (return ()) (e :: Exception.IOException)

main :: IO ()
main =
   do
      debugFileName <- getDebugFileName
      handle <- openFile debugFileName ReadMode
      output handle
