{- Test file for FileSys.hs.  We create a small file system and
   do a few things with it.
   
   The repository comes from CVSROOT.  
   currentDirectory/A is used for the FileSys working
   directory.  The real files are in currentDirectory/B.
   The versions go into currentDirectory/1,2,3...

   To run this test, cd test1dir and to ./totest.  The directory
   names in ./totest will need some hacking and you will need
   to set up a CVS repository - sorry . . .
   -}
module Main(main) where

import IO
import Directory
import System

import FileNames

import InfoBus

import UniTypes
import FileSys

main =
   do
      directory <- getCurrentDirectory
      hostString <- getEnv "CVSROOT"
      let
         (+++) = combineNames
         workingDir = directory +++ "A"
         ourDir = directory +++ "B"
         parameters = [HostString hostString,WorkingDir workingDir]

      makeFileSys parameters

      fileSys <- connectFileSys parameters
      let 
         typeDataBase = getTypeDataBase fileSys
      textType <- registerType typeDataBase 
         (UniTypeData{name="Text File",extension="txt"})

      createDirectory ourDir
      let
         our fileName = ourDir +++ fileName
         writeTxt fname str =
            writeFile(our(makeFileName fname textType)) str
         commit vers changes = commitVersion fileSys vers ourDir changes 

      createDirectory (our "foo")
      writeTxt "bar" "bar"
      writeTxt ("foo"+++"baz") "baz"
      writeTxt ("foo"+++"bat") "bat"

      [version1] <- getVersions fileSys
 
      let
         foo = ["foo"]
         bar = ["bar.txt"]
         baz = ["foo","baz.txt"]
         bat = ["foo","bat.txt"]
         boz = ["boz.txt"]
      
      version2 <- commit version1 [
         NewFile bar,
         NewFolder foo,
         NewFile baz,
         NewFile bat
         ]


      writeTxt ("foo"+++"bat") "bat2"
      version3 <- commit version2 [
         ChangeFile bat
         ]
 
      writeTxt "bar" "bar2"
      writeTxt ("foo"+++"baz") "baz2"

      version4 <- commit version3 [
         ChangeFile bar,
         ChangeFile baz,
         MVObject baz boz,
         RMObject foo
         ]

      top2 <- getTop fileSys version2
      barObj2 <- lookupLocalFilePath fileSys top2 "bar.txt"
      barAtts <- extractFileAttributes fileSys barObj2

      batObj2 <- lookupLocalFilePath fileSys top2 "foo/bat.txt"
      batAtts <- extractFileAttributes fileSys batObj2

      let
         barAtts' = setAttribute barAtts "BAR" (Just 2)
         barAtts'' = setAttribute barAtts' "FOO" (Just "BAR")
         batAtts' = setAttribute batAtts "FOO" (Just "BAZ")
      
      version22 <- commit version2 [
         ChangeAttributes bar barAtts'',
         ChangeAttributes bat batAtts'
         ]

      let
         output version str =
            do
               top <- getTop fileSys version
               extractFileObj fileSys top (directory +++ str)
      output version1 "1"
      output version2 "2"
      output version22 "22"
      output version3 "3"
      output version4 "4"

      top22 <- getTop fileSys version22
      barObj22 <- lookupLocalFilePath fileSys top22 "bar.txt"
      barAtts <- extractFileAttributes fileSys barObj22

      batObj22 <- lookupLocalFilePath fileSys top22 "foo/bat.txt"
      batAtts <- extractFileAttributes fileSys batObj22
      putStrLn(show (barAtts,batAtts))
      putStrLn "Test completed"
      shutdown
      putStrLn "Shutdown completed"
                   
      
   
      