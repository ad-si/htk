{- This module is supposed to provide simple implementions of
   the functionality used by the tools in the original WB in UniForM,
   without needing to use the OMS.  If possible.
   -}

module WB(
   getWBToolFilePath,
   interactor,newInterActor,stop, -- from InterActor
   toolFailed, -- from SIMClasses
   text, -- from HasText
   IA, -- from ExternalEvent
   linemode,arguments,environment,workingdir,standarderrors,pollinterval,
      PosixProcess, -- from ChildProcess

   module Object,
   module Selective,
   module Computation,
   module InfoBus
   ) where

import Object
import Selective
import Computation
import Debug(debug)
import ExternalEvent(IA)
import InterActor(interactor,newInterActor,stop)
import ChildProcess(linemode,arguments,environment,workingdir,standarderrors,pollinterval,PosixProcess)
import InfoBus
import SIMClasses(toolFailed)

import GUIBaseClasses(HasText(..))

getWBToolFilePath :: String -> IO String
getWBToolFilePath s = return s