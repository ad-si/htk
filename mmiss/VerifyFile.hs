{- This is used for testing only -}
module VerifyFile(verifyFile) where

import IO

import XmlParse
import XmlTypes

import MMiSSDTD

verifyFile :: FilePath -> IO ()
verifyFile fpath =
   do 
      h <- openFile fpath ReadMode
      s <- hGetContents h
      let (XmlTypes.Document _ _ el) = xmlParse fpath s
      putStrLn (show (validateElement "package" el))
    