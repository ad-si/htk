module Main (
        main 
        ) 
where

import Concurrency
import Object
import Debug(debug)


main :: IO ()
main =  
        newChannel                                      >>= \ ch ->
        newChannel                                      >>= \ ch2 ->
        newChannel                                      >>= \ ch3 ->
        newMVar ""                                      >>= \ av ->
        newObject                                       >>= \ oid ->
        let ev = EventID oid "Hello" in
                consumer ch ch2 ch3 av ev               >>= \ iact ->
                producer ch ch2 ch3 av ev iact          >>
                block

consumer :: Channel Int -> Channel Int -> Channel () -> MVar String -> EventID -> IO InterActor         
consumer ch ch2 ch3 av ev =
        interactor (\iact -> 
                tryEV (fetch ev >>> exit) >>> putStr "Caught Event\n"
          +>    (tryEV (receive ch2 >>> exit) >>> putStr "Caught Channel Event\n")
          +>    event (return (receive ch3 >>> putStr "Hanging On\n"))
          +>    receive ch >>> putStr "Got Message\n"
         )
        where  fetch :: EventDesignator e => e -> EV ()
               fetch ev = listen ev subs subs 
               subs iact = done 
               exit = raise conditionFailed

producer ch ch2 ch3 av ev iact =
        delay (secs 2)                                          >>
        readLine'                                               >>= \ c ->
        case c of {
{-
         "b" ->
                putStr "Broadcasting\n" >>
                setVar av "B"   >>
                producer ch ch2 ch3 av ev iact;
-}
         "c"  ->
                putStr "Sending\n "     >> 
                sendIO ch2 1            >>
                producer ch ch2 ch3 av ev iact;
         "e" ->
                putStr "Notifying\n"    >>
                notify iact ev ()       >>
                producer ch ch2 ch3 av ev iact;
         "s"  ->
                putStr "Sending\n "     >> 
                sendIO ch 1     >>
                producer ch ch2 ch3 av ev iact;
         "h"  ->
                putStr "Hangup\n "      >> 
                sendIO ch3 ()           >>
                producer ch ch2 ch3 av ev iact;
          x ->  done;
         }


readLine' =
        getChar >>= \c ->
        if c == '\n' then
                return ""
        else 
                readLine' >>= \ l ->
                return (c : l)

