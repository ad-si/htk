{- #########################################################################

MODULE        : GraphEditor
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : A GraphEditor


   ######################################################################### -}


module Main (
        main

        ) where

import Computation
import HTk
import DaVinci
import GraphEditor
import PulldownMenu
import Frame
import DialogWin
import Debug(debug)

main = do
        gui <- htk [{-logfile (1::Int)-}]
        dav <- davinci []
        setErrorHandler err
{-
        b <- newMenuButton [text "Pop-up Menu"]
        mn <- newMenu [tearOff On, parent (b::MenuButton ())]
        button [text "Create",parent (mn::Menu ())]
        button [text "Delete",parent mn]
        button [text "Edit",parent mn]

        win <- window b []

        sync(receive b)
-}

        newGraphEditor dav
        interactor(\iact -> 
                destroyed dav       >>> do {destroy gui; stop iact}
           +>   lastGraphClosed dav >>> do {destroy gui; destroy dav; stop iact}
                )
        done 

err e = newErrorWin (show e) []

