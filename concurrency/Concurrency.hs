{- #########################################################################

MODULE        : Concurrency
AUTHOR        : Einar Karlsen, Germany
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : The kernel of the Concurrency ToolKit


   ######################################################################### -}


module Concurrency (
         MVar(..),

         module Thread,
         module Variable,
         module Lock,
         module BSem,
         module QSem,
         module Mutex,
         module RVar,
         module Event,
         module Selective,
         module SAP,
         module Future,

         forkIOnull,-- what used to be forkIO - forking returning () rather
                    -- than a threadId.
         block      -- block forever (used in the main thread to keep the
                    -- process going)

        ) where

import Thread
import Variable
import Lock
import Event
import Selective
import Mutex
import RVar
import BSem
import QSem
import SAP
import Future

import Debug(debug)


forkIOnull :: IO() -> IO()
forkIOnull action =
   do
      _ <- forkIO action
      return ()

block :: IO()
block = deadlock
