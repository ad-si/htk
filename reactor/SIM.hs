{- #########################################################################

MODULE        : Reactor
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : The Subsystem Interaction Manager. 

TO BE DONE    : C-Callbacks, i.e. calling Haskell from C, are missing.


   ######################################################################### -}


module SIM (
        module Concurrency,
        module EventLoop,
        module Event,
        module InterActor,
        module EventStream,

        module SIMClasses,
        module Controller,
        module InfoBus,
        module Signal,

        module Collector,
        module Observer,
        module Dispatcher,

        PosixProcess,
        linemode,
        arguments,
        environment,
        workingdir,
        standarderrors,
        pollinterval,
        defaultPosixProcess

        ) where

import Concurrency
import Interaction
import InterActor
import Controller
import EventStream
import EventLoop
import Event

import Dynamics(Dyn,Typeable(..),TypeTag)
import SIMClasses
import InfoBus
import Signal
import Collector
import Observer
import Dispatcher

import ChildProcess
import Debug(debug)

                
