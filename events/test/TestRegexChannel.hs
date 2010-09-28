{- TestRegexChannel tests regex channels.  -}
module Main(main) where

import Events.Events
import Events.RegexChannel
import Events.Spawn

main =
  do
     chan <- newRegexChannel
     let
        sendStr str = sync(sendString chan str)
        sendStr1 str = sync(noWait(sendString chan str))
        readLines =
           do
              line <- getLine
              sendStr line
              readLines
        puts str = always (putStrLn str)
        mE str = matchEvent str chan
        mA = matchAny chan

        readFoos =
              (do
                 mE "quit"
                 puts "Quitting"
              )
           +> (do
                 mE "foo"
                 puts "Matched foo"
                 readFoos
              )
           +> (do
                 mE "bar"
                 puts "Switch baz"
                 readBaz
              )
           +> (do
                 str <- mA
                 puts ("Read "++str)
                 readFoos
               )
        readBaz =
              (do
                 mE "baz"
                 puts "Switch foos"
                 readFoos
              )
           +> (do
                 str <- mA
                 puts ("Read "++str)
                 readFoos
               )

        readAny =
           do
               do
                 str <- mA
                 puts ("Read "++str)
                 readAny


     sendStr1 "foo"
     sendStr1 "bar"
     sendStr1 "baz"
     spawn(readLines)
     sync(readFoos)
