{- #########################################################################

MODULE        : Test6
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha

   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import SIM
import Dynamics

import ScrollBox
import LabelBox

import Label
import Entry
import Message
import Separator
import Button
import CheckButton
import RadioButton
import MenuButton
import PulldownMenu
import RadioGroup
import SpinButton
import Scale
import Frame
import ListBox
import ScrollBar
import Space
import ComboBox
import ScrollBox
import Editor
import PromptWin

import DialogWin

import Colour
import Keyboard
import Mouse
import Bell
import Index

import ExtendedPrelude
import Directory
import InputForm
import InputWin

import Concurrency
import qualified Selective
import qualified Event

import TestUtil (logout)
import Debug(debug)



main = do {
        print ((read "0") :: Double);
        htk [{- logfile (1::Int), -}];
        setErrorHandler errWin;
        testEmitEvent;

        done
} where errWin e = newErrorWin ("Haskell-Tk error occured: " ++ show e)[modal True]


testEmitEvent = do {
        b <- newVFBox [];
        e <- newEntry [value (0::Int),parent b];
        bt <- button [text "emit",parent b];
        bt2 <- button [text "flash",parent b];
        w <- window b [text "Emitter"];
        forkIO (emitter bt);
        controller' w (
                (Selective.receive bt) Event.>>> do{updValue e succ; done}
           +>   (Selective.receive bt2) Event.>>> do{forkIO(do{flash bt;bell});done}
                )
} where emitter bt = do {
                print "calling invoke";
                ans <- try(invoke bt);
                checkError ans;
                print "calling delay";
                ans <- try (delay (secs 0.05));
                checkError ans;
                emitter bt
                }
        checkError (Right _) = done
        checkError (Left e) = 
                error (show e)
