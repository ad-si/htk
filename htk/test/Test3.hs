{- #########################################################################

MODULE        : Test3
AUTHOR        : Einar Karlsen,  
University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}


module Main (main
        ) 
where

import Concurrency
import Dynamics
import HTk

import Counter
import Prompt
import RadioGroup
import DialogWin

import Char(isDigit)

import Mouse
import Screen
import Printer
import Index
import Selection
import ICursor
import Slider
import Icon
import Keyboard
import Bell

import Frame
import Box
import Label
import Image
import BitMap
import Message
import Entry
import ListBox
import Scale
import ScrollBar
import Button
import RadioButton
import CheckButton
import MenuButton
import Separator
import Menu
import PulldownMenu


import Canvas
import CanvasItem
import CanvasTag
import Arc
import Line
import Oval
import Polygon
import Rectangle
import EmbeddedCanvasWin
import TextItem
import BitMapItem
import ImageItem

import Editor
import TextTag
import EmbeddedTextWin

import TestUtil (logout)
import Debug(debug)



main :: IO ()
main = 
   do
      htk [{-logfile (1::Int) -}]
      mousetracker
      testcounter1
      testcounter2  
      testmenu
      testutil
      block


-- --------------------------------------------------------------------------
-- Mouse Tracker =
-- --------------------------------------------------------------------------           
mousetracker =
        newCanvas [bg white, flexible]                      >>= \cv ->
        window cv [text "Mouse Test", size (cm 15,cm 20)] >>= \win ->
        newPrompt [text "Motions", value (0::Int)]              >>= \pr ->
        newEmbeddedCanvasWin (pr :: Prompt Int)[
                position (200,300),
                anchor West,
                parent cv
                ]                                       >>
        controller' win ( trackCursor cv pr)            >>
        done
        where isInt [] = False
              isInt str = (filter (\c -> not (isDigit c)) str) == []
                

trackCursor :: Canvas -> Prompt Int  -> IA ()
trackCursor cv pr =
           userinteraction cv Motion Request >>> do {updValue pr incr; done}
        +> userinteraction cv Enter Request >>> do {background "gold" cv; done}
        +> userinteraction cv Leave Request >>> do {background white cv; done}
        where   incr :: Int -> Int
                incr x = x + 1


-- --------------------------------------------------------------------------
-- TEST: Counter
-- --------------------------------------------------------------------------           
testcounter2 =
        newVBox []                                                      >>= \box ->
        newLabel [value "Counter 1",parent box,justify JustCenter]      >>
        newCounter [parent box,flexible]                                >>= \c1 ->
        newLabel [value "Counter 2",parent box,justify JustCenter]      >>
        newCounter [parent box,flexible]                                >>= \c2 ->
        newLabel [value "Counter 3",parent box,justify JustCenter]      >>
        newCounter [parent box,flexible]                                >>= \c3 ->
        button  [text "Quit", parent box,flexible]              >>= \b ->
        window box [text "Multiple Counters"]                   >>= \ win ->
        controller' win (inaction :: IA ())                     >>= \ iact ->
        interactor (\iact -> (triggered b +> destroyed b )>>> stop iact)        >>
        controller' c1 (triggered c1 >>> done)                  >>
        controller' c2 (triggered c2 >>> done)                  >>
        controller' c3 (triggered c3 >>> done)                  >>
        done

        
testcounter1 =
        sequence [ script i | i <- [1,2,3]]                   >>= \ cnts ->
        terminator cnts                                         >>
        done

script i =
        newCounter []                                           >>= \ cnt ->
        window cnt [text ("Counter" ++ (show i))]               >>= \ win ->
        newChannel                                              >>= \ ch ->
        controller win ( wmCounter win cnt ch)                  >>= \ iact ->
        return (i,cnt,iact,ch)
                

wmCounter :: Window -> Counter Int -> Channel () -> InterActor -> IA ()
wmCounter win cnt ch iact = 
           triggered cnt >>> done
        +> receive ch |>> become iact (destroyed win >>> do {stop iact;done})


terminator :: CounterInf -> IO ()
terminator cnts = 
        newHBox []                                              >>= \ box ->
        sequence (fmap (makeButt box) cnts)                    >>= \ bts ->
        button [text "Quit", parent box]                        >>= \ bq ->
        window box [text "Terminator"]                          >>= \ win ->
        controller' win ( terminate bts bq)                     >>= \ iact ->
        done
        where ctitle i = "Counter" ++ (show i)
              makeButt box (i,cnt,iact,ch) = 
                button [text (ctitle i), parent box]            >>= \ butt ->
                return (triggered butt >>> forkIOnull (sendIO ch ()))


terminate :: [IA ()] -> Button () -> IA ()
terminate evhl bq = 
            choose evhl
        +>  triggered bq >>> do {forkIOnull logout; done} 


type CounterInf = [(Int,Counter Int,InterActor,Channel ())]

-- --------------------------------------------------------------------------
-- TEST: Menu
-- --------------------------------------------------------------------------           
testmenu :: IO ()
testmenu =
        newMenuButton [text "Commands"]                         >>= \ but ->
        newPulldownMenu (but::MenuButton ()) []                 >>= \ mn ->
        newSeparator [parent (mn :: (Menu ()))]                 >>
        radiobutton' [text "Yes",parent mn]                     >>
        radiobutton' [text "No",parent mn]                      >>
        radiobutton' [text "Maybe",parent mn]                   >>      
        newSeparator [parent mn]                                        >>
        checkbutton'[text "Auto Save", parent mn]               >>      
        checkbutton'[text "Versioning", parent mn]              >>      
        newSeparator [parent mn]                                        >>
        button [text "Show", parent mn]                         >>
        button [text "Edit", parent mn]                         >>
        button [text "Query", parent mn]                                >>
        button [text "Search", parent mn]                       >>
        button [text "Substitute", parent mn]                   >>
        button [text "Mail", parent mn]                         >>
        newSeparator [parent mn]                                        >>
        button [text "Proof", parent mn]                                >>      
        button [text "Transform", parent mn]                    >>      
        button [text "Library", parent mn]                      >>      
        button [text "Configurator", parent mn]                 >>      
        button [text "Version Manager", parent mn]              >>      
        button [text "Development Manager", parent mn]          >>      
        newSeparator [parent mn]                                        >>
        button [text "Tool1", parent mn]                                >>      
        button [text "Tool2", parent mn]                                >>      
        button [text "Tool3", parent mn]                                >>      
        button [text "Tool4", parent mn]                                >>      
        button [text "Tool5", parent mn]                                >>      
        button [text "Tool6", parent mn]                                >>      
        button [text "Tool7", parent mn]                                >>      
        button [text "Tool8", parent mn]                                >>      
        button [text "Tool9", parent mn]                                >>      
        button [text "Tool10", parent mn]                       >>      
        button [text "Tool11", parent mn]                       >>      
        button [text "Tool12", parent mn]                       >>      
        button [text "Tool13", parent mn]                       >>      
        window but [text "Controller"]                          >>= \ win ->
        controller' win inaction                                >>
        done
        where   radiobutton' :: [Config (RadioButton ())] -> IO (RadioButton ())
                radiobutton' cnfs = do {
                        b <- newRadioButton cnfs;
                        mapTrigger (\_ -> return ()) b
                        }
                checkbutton' :: [Config (CheckButton ())] -> IO (CheckButton ())
                checkbutton' cnfs = do {
                        b <- newCheckButton cnfs;
                        mapTrigger (\_ -> return ()) b
                        }

        
                
-- --------------------------------------------------------------------------
-- Menu
-- --------------------------------------------------------------------------           
menu1 =
        newVBox []                                                      >>= \ box ->
        newLabel [value "",fill Horizontal, parent box, relief Sunken]  >>= \ lbl ->
        newMenuButton [text "Find",parent box]                  >>= \ mb ->
        newPulldownMenu (mb::MenuButton ()) [tearOff Off]                               >>= \ mn ->
        button [text "Find and Replace ...", parent mn]         >>= \ me1 ->    
        newMenuButton [text "Find Selection", parent mn]                >>= \ mb2 ->
        newPulldownMenu (mb2::MenuButton ()) [tearOff Off]                              >>= \ smn  ->
        button [text "Forward", parent smn]                     >>= \ me2 ->    
        button [text "Backward", parent smn]                    >>= \ me3 ->    
        button [text "Find Marked Text ...", parent mn]         >>= \ me4 ->    
        button [text "Replace Field", parent mn]                        >>= \ me5 ->
        window box [text "Test"]                                >>= \ win ->
        controller' win (
                triggered me1 >>> do { value "Find and Replace" lbl; done}
           +>   triggered me2 >>> do { value "Forward" lbl; done}
           +>   triggered me3 >>> do { value "Backward" lbl; done}
           +>   triggered me4 >>> do { value "Find Marked Text" lbl; done}
        )                                                       >>
        configure mn [background blue]                          >>
        configure smn [background red]                          >>      
        configure smn [background red]                          >>      
        configure me1 [background green]                        >>
        done


-- --------------------------------------------------------------------------
-- Event Loops (almost the traditional way)
-- --------------------------------------------------------------------------           
testutil = do {
        newAlertWin "Press button to continue test" [modal True]

--      win <- newAlertWin "Press button to continue test" [modal True];
--      sync (triggered win)
-- it seems as if HTK has changed.  newAlertWin (like the other
-- dialog window actions) won't return until the user has clicked the
-- button. 
        }


-- --------------------------------------------------------------------------
-- Choice Button
-- --------------------------------------------------------------------------   

white = toColour "white"
blue = toColour "blue"
red = toColour "red"
green = toColour "green"

