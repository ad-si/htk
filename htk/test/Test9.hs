{- #########################################################################

MODULE        : Test2
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget


   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import Concurrency
import Counter
import Bell
import PulldownMenu
import GUIObject

import IO(stdout)
import Debug(debug)


main = do
        htk []
        setLogFile (Just stdout)

        cnt1 <- newCounter [value 0, cursor "circle"]
        win1 <- window cnt1 [text "Counter1"]
        controller' win1 (triggered cnt1 >>> bell)

        cnt2 <- newCounter [value 0]
        win <- window cnt2 [text "Counter2"]
        interactor (\iact ->
                 triggered cnt2 >>> done
             +>  destroyed win >>> (print "CLOSING" >> become iact (inaction::IA ()))
            )

        mbt <- newMenuButton [text "Hello"]
        mn <- newPulldownMenu (mbt::MenuButton ()) [tearOff On]
        bt <- button [text "Howdy",command (\() -> bell)]
        configure bt [parent (mn::Menu ())] 
        interactor (const(triggered bt >>> done))
        delay(secs 1)
        win <-  window mbt [text "Button"]
        (GUIOBJECT _ mv) <- return (toGUIObject bt)
        o <- getVar mv
        case bindings o of
                [] ->  (error "no bindings")
                _ ->  done


