{- Fix feature of HaXml output by which close-tags are sometimes
   preceded by spaces, which causes some tools to complain (even though
   it's legal XML).
   -}
module XmlFix(
   xmlFix, -- :: String -> String
   ) where

import Char

import Computation (done)
import StringSkip


xmlFix :: String -> String
xmlFix str1 =
   let
      ((),str2) = runStringSkip fix str1
   in
      str2

-- The trickiest part is that we need to skip quotes, processing instructions,
-- comments or CDATA sections.
fix :: StringSkip ()
fix =
   do
      end <- atEnd
      if end
         then
            done
         else
            do
               skipped <- skipPairs [("\"","\""),("\'","\'"),("<!--","-->"),
                  ("<?","?>"),("<![CDATA[","]]>")]
               if skipped
                  then
                     done
                  else
                     doOneChar
               fix
 
doOneChar :: StringSkip ()
doOneChar =
   do
      spaces <- doSpaces ""

      nextGT <- match ">"
      dropSpaces <- if nextGT
         then
            return True
         else
            match "/>"

      if dropSpaces
         then
            done
         else
            insertBefore spaces
      copyOrEnd
      done

doSpaces :: String -> StringSkip String
doSpaces spaces =
   do
      spaceOpt <- skipIf isSpace
      case spaceOpt of
         Nothing -> return (reverse spaces)
         Just space -> doSpaces (space : spaces)


skipPairs :: [(String,String)] -> StringSkip Bool
skipPairs [] = return False
skipPairs ((b,e) : rest) =
   do
      skipThis <- matchCopy b
      if skipThis
         then
            do
               copyAfter e
               return True
         else
             skipPairs rest