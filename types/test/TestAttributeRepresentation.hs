module Main(main) where

import IO

import Int
import Exception

import Computation

import CodedValue

import CodedValueStore

stringify :: CodedValue -> [String]
-- crude representation of a coded value as a list of records.
-- There is always at least one record.
stringify (CodedValue l) = 
   case l of
      [] -> [""]
      x : xs -> 
         let
            rest = stringify (CodedValue xs)
         in
            case x of
               Nothing -> "" : rest
               Just ch ->
                  let
                     r:rs = rest
                  in
                     (ch:r):rs

testValue :: (HasCodedValue value,Show value,Eq value) => value -> IO ()
testValue value =
   catchFormatError (
      do
         let repository = error "Repository not defined"
         encoded <- doEncodeIO value repository
         str <- toObjectSource encoded
         encoded2 <- fromObjectSource str
         decoded <- doDecodeIO encoded repository

         if decoded == value
            then
               done
            else
               message
                  ("Mismatch on "++(show value)++" yielding "++
                     (show decoded) ++ " with " ++ 
                        (show (stringify encoded)) ++ "\n")
      )
      (\ mess ->
         message
            ("Couldn't decode value coming from " ++
               (show value) ++ " message " ++ mess ++ "\n")
         )

main =
   do
      message "T1"
      testValue ()
      message "T2"
      testValue "\0"
      message "T3"
      testValue 'T'
      message "T3a"
      testValue True
      message "T4"
      testValue (0::Int)
      message "T5"
      testValue (20::Int64)
      message "T6"
      testValue (-64::Int8)
      message "T7"
      testValue (314159265::Int)
      message "T8"
      testValue (Nothing :: Maybe Bool)
      message "T9"
      testValue (Just False)
      message "T10"
      testValue ((Left 9999) :: Either Int Bool)
      message "T11"
      testValue ([Left 9999,Right (919,"foo"),Left 64,Right (99999999,"bar"),
         Left 63,Right (919,"")] :: [Either Int (Int,String)])
      message "T12"
      testValue (Just False,3 :: Int,"Hello",
         (Nothing :: Maybe String,Just "F",Just (Just "G")))
      message "T13"
      testValue ([] :: [Int])
      message "T14"
      testValue "Fooo\nBar\\\n\n"
      message "Test Completed"

message :: String -> IO ()
message str =
   do
      hPutStr stdout str
      hFlush stdout