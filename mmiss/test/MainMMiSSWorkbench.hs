{- Runs the MMiSS workbench as a client. -}
module Main(main) where

#include "config.h"

import System

import Posix

import Debug(debug)
import WBFiles
import Computation

import Events
import Destructible
import InfoBus

#if (WORK_AROUND_BDB_LINUX_BUG != 0)
import SysVars
#endif

import HTk
import DialogWin

import DaVinciGraph

import VersionGraph

import MMiSSInitialise

main =
   do
      parseArgumentsRequiring [
         "top",
         "server",
         "editor"
         ]

#if (WORK_AROUND_BDB_LINUX_BUG != 0)
      unSetEnv "MALLOC_CHECK_"
#endif

      withdrawWish
      seq loadHTkImages done 
      repository <- mmissInitialise
      versionGraph <- newVersionGraph daVinciSort repository
      sync (destroyed versionGraph)
      cleanupWish
      exitImmediately ExitSuccess

