{- This is used for testing only -}
module VerifyFile(verifyFile) where

import IO

import XmlParse
import XmlTypes

import DTD_MMiSS
import MMiSSVerify

verifyFile :: FilePath -> IO ()
verifyFile fpath =
   do 
      let
         package = toDTDItem (error "Bar" :: Package) 
      h <- openFile fpath ReadMode
      s <- hGetContents h
      let (XmlTypes.Document _ _ el) = xmlParse fpath s
      sOpt <- verifyDTDItem package el
      putStrLn (show sOpt)
    