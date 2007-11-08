{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

import IO
import System

import Computation

import MMiSSDTD


main :: IO ()
main =
   do
      args <- System.getArgs
      let
         expected = case args of
            [] -> "package"
            [arg] -> arg

      doc <- getContents
      elEither <- xmlParseCheck "stdin" doc
      el <- case  fromWithError elEither of
         Left str -> ioError (userError str)
         Right str -> return str
      let verified = validateElement expected el
      case verified of
         [] -> done
         errors -> error (unlines errors)
{-
   -- obsolete for now (it depends on obsolete MMiSSContent module).
      let
         structured = coerceWithError (structureContents el)
         (contents1 :: [Content]) = contents (accContents structured)
      coded1 <- doEncodeIO contents1 undefined
      putStrLn (toString coded1)
      (contents2 :: [Content]) <- doDecodeIO coded1 undefined
      putStrLn (render (vcat (map content contents2)))
-}
