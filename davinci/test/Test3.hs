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

import HTk
import DaVinci
import Debug(debug)

main = do 
        gui <- htk [{- logfile (1::Int) -} ]

-- NEW (deactivate and activate have been moved into the next line.
-- Hopefully this will at least compile)
        dav <- davinci [deactivate,activate]


        g <- newGraph [text "Folder",
                graphorientation LeftToRight
                ]



        configure g [showmsg "Building Graph"]

        r <- newNode g Nothing [text "root"]

        foreach [1..100] (\i -> do {
                n <- newNode g Nothing [text (show i)];
-- NEW (again to get it to compile) we need to add a
-- type for the return of getText so that ghc can work
-- out what instance of HasText to use.  Actually this one
-- might even be right.
                (getText n)::IO String;
                newEdge Nothing r n [];
                })
        

        configure g [showmsg "Build Graph"]

        v <- displayGraph g

        configure g [showmsg "Finished"]

        interactor(\iact -> 
                destroyed dav       >>> do {destroy gui; stop iact}
           +>   lastGraphClosed dav >>> do {destroy gui; destroy dav; stop iact}
           +>   destroyed g         >>> do {destroy gui; stop iact}
                )

        done 

