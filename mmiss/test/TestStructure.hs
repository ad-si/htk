{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

import IO

import Pretty

import XmlTypes
import XmlParse
import XmlPP

import Computation
import AtomString

import CodedValue

import MMiSSContent
import MMiSSDTD


main =
   do
      doc <- getContents
      let 
         (Document _ _ el) = xmlParse "Foo" doc
      verified <- validateElement "package" el
      case verified of
         [] -> done
         errors -> error (unlines mess)
      let
         structured = structureContents el
         (contents1 :: [Content]) = contents (accContents structured)
      coded1 <- doEncodeIO contents1 undefined
      putStrLn (toString coded1)
      (contents2 :: [Content]) <- doDecodeIO coded1 undefined
      putStrLn (render (vcat (map content contents2)))