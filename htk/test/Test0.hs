{- #########################################################################

***** This test no longer compiles.  The reason is that the
?>>= operator is no longer implemented (it used to be in Einar's
file Event.hs but has long disappeared).  
    George Russell (October 1999)


MODULE        : TestSystem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}

module Main (
        main 
        ) 
where

import HTk
import Concurrency

import Counter
import Prompt
import DialogWin
import Font


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

import Posix(getEnvVar)

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

import TestUtil
import Debug(debug)

main :: IO ()
main = 
        htk [{- logfile 1 -}]                                   >>
        setErrorHandler errWin                                  >>
        testEAC                                                 >>
        testScreen disp                                         >>
        testBell                                                >>
        testMouse                                               >>
        testKeyboard                                            >>
        testBitMap                                              >>
        testImage                                               >>
        testSlider                                              >>
        testIcon                                                >>
        testICursor                                             >>
        testSelection0                                          >>
        testWindow                                              >>
        testEventLoop                                           >>
        testRemoteWindow disp                                   >>      
        testTimeout                                             >>
        testfocus                                               >>
        testPacker1                                             >>
        testPacker2                                             >>
        testPacker3                                             >>
        testPacker4                                             >>
        done
        where disp = "hydra:0.0"
              errWin e = newErrorWin ("Haskell-Tk error occured: " ++ show e)[]


-- --------------------------------------------------------------------------
-- Screen
-- --------------------------------------------------------------------------                   
testScreen :: String -> IO ()
testScreen disp =
        newImage [filename "~ewk/www/ewk.gif"]                  >>= \ img ->
        newLabel [photo img]                                    >>= \ lbl ->
        window (lbl :: Label Image) [text "Test Screen"]        >>= \ win1 ->
        controller' win1 (inaction :: EV ())                    >>
        runtest win1                                            >>
        newImage [filename "~kol/www/kol_small.gif"]            >>= \ img ->
        newLabel [photo img]                                    >>= \ lbl ->
        window (lbl :: Label Image) [text "Test Remote Screen", screen disp]    >>= \ win2 ->
        controller' win2 (inaction :: EV ())                    >>
        runtest win2                                            >>
        done
        where runtest win =
                fetchC scrn getScreenHeight                     >>
                fetchC scrn getScreenWidth                      >>
                fetchC scrn getScreenVisual                     >>
                fetchC scrn getScreenManager                    >>
                done
                where scrn = Screen win



-- --------------------------------------------------------------------------
-- Bell
-- --------------------------------------------------------------------------           
testBell = do {
        bell;
        l <- newLabel [value ""];
        win <- window l [text "Testing Bell"];
        controller' win inaction;
        ringBell (Just win)
        }


-- --------------------------------------------------------------------------
-- Mouse
-- --------------------------------------------------------------------------           
testMouse = do {
        l <- newEditor [size (20,20), bg "white",value (0::Int),disable,flexible];
        win <- window l [text "Test Mouse"];
        value 10 l;
        errorWin "start";
        controller' win (
                mouseEnter l >>> do {bg "blue" l; done}
           +>   mouseLeave l >>> do {bg "white" l; done}
--         +>   mouseMotion l >>> do {updValue l succ; done}
           +>   mouseMotion l >>> do {forkIO(errorWin "Ack"); done}
--         +>   mouseMotion l >>> bell
           +>   mouseButtonPress l 1 >>> bell
           +>   mouseButtonRelease l 1 >>> bell
                )
        } where errorWin str = do {
                        b <- button [text str];
                        w <- window b [text str];
                        try(sync(receive b));
                        destroy w
                        }
                        

-- --------------------------------------------------------------------------
-- Keyboard
-- --------------------------------------------------------------------------           
testKeyboard = do {
        l <- newEditor [size (30,20), bg "white",value "",flexible];
        win <- window l [text "Test Keyboard"];
        controller' win (
                anyKeyPressed l >>>= \(pos,key) -> do {
                        appendText l (show pos ++ " " ++ show key ++ "\n")
                        }
                )
        } 


-- --------------------------------------------------------------------------
-- Insertion Cursor
-- --------------------------------------------------------------------------           
testICursor = do {
        b <- newHFBox [];
        l <- newEntry [bg "white",value "testing icursor",flexible,parent b];
        win <- window b [text "Test Insertion Cursor"];
        insertionCursor (3::Int) l;
        testICursor l;
        l1 <- newEntry [bg "white",value "testing icursor",flexible,parent b];
        insertionCursor (3::Int) l;
        setICursor l1;
        testICursor l1;
        l2 <- newEditor [bg "white",value "testing icursor",flexible,parent b];
        insertionCursor (3::Int) l;
        setICursor l2;
        testICursor l2;
        done
        
} where testICursor :: HasInsertionCursor w => w -> IO ()
        testICursor l = do {
                testHasHeight (ICursor l);
                testHasWidth (ICursor l);
                testHasBorder (ICursor l);
                testHasColour (ICursor l);
                }
        setICursor :: HasInsertionCursor w => w -> IO ()
        setICursor l = do {
                setSize (ICursor l);
                setWidgetColour (ICursor l);
                configure (ICursor l) [borderwidth 1, relief Sunken];
                done
                }


-- --------------------------------------------------------------------------
-- Selection
-- --------------------------------------------------------------------------           
testSelection0 = do {
        b <- newHFBox [];
        l <- newEntry [bg "white",value "testing selection",flexible,parent b];
        win <- window b [text "Test Selection"];
        runSelection l;
        testSelection2 l;
        l1 <- newEntry [bg "white",value "testing selection",flexible,parent b];
        insertionCursor (3::Int) l;
        setSelection l1;
        runSelection l1;
        testSelection2 l1;
        l2 <- newEditor [bg "white",value "testing selection",flexible,parent b];
        insertionCursor (3::Int) l;
        setSelection l2;
        runSelection l2;
        done
        
} where runSelection :: (Widget w,HasSelection w) => w -> IO ()
        runSelection l = do {
                testHasBorder (Selection l);
                testHasColour (Selection l);
                }
        setSelection :: (Widget w,HasSelection w) => w -> IO ()
        setSelection l = do {
                setWidgetColour (Selection l);
                configure (Selection l) [borderwidth 1, relief Sunken];
                done
                }
        testSelection2 w = do {
                selection (3::Int) w;
                selectionRange (3::Int) (5::Int) w;
                isSelected w (5::Int);
                (getSelectionStart w :: IO (Maybe Int));
                (getSelectionEnd w :: IO (Maybe Int));
                (getSelectionRange w :: IO (Maybe (Int,Int)));
                }


-- --------------------------------------------------------------------------
-- Slider
-- --------------------------------------------------------------------------           
testSlider = do {
        b <- newHFBox [];
        l <- newScale [bg "white",flexible,parent b];
        win <- window b [text "Test Slider"];
        runSliderTest l;
        l1 <- newScale [bg "white",flexible,parent b];
        setSlider l1;
        runSliderTest l1;
        l2 <- newScrollBar [bg "white",flexible,parent b];
        setSlider l2;
        runSliderTest l2;
        done
        
} where runSliderTest :: HasSlider w => w -> IO ()
        runSliderTest l = do {
                fetchC (Slider l) getRepeatInterval;
                fetchC (Slider l) getRepeatDelay;
                testHasColour (Slider l);
                }
        setSlider :: HasSlider w => w -> IO ()
        setSlider l = do {
                repeatInterval 1 (Slider l);
                repeatDelay 1 (Slider l);
                setWidgetColour (Slider l);
                done
                }



-- --------------------------------------------------------------------------
-- Icon
-- --------------------------------------------------------------------------           
testIcon = do {
        m <- newMessage [value "testing the config options related to icons"];
        w <- window m [text "Test Icon"];
        runTest w;
        configure (Icon w) [position (20,20), text "icon", bitmap hourglass,
                        iconMask hourglass
                        ];
        runTest w;
} where runTest w = do {
                testHasPosition (Icon w);
                testHasText (Icon w);
                testHasBitMap (Icon w);
                getIconMask (Icon w);
                done
                }



-- --------------------------------------------------------------------------
-- Packer
-- --------------------------------------------------------------------------                   
testPacker1 :: IO ()
testPacker1 =
        newGUIEventLoop                                         >>= \ el ->
        newVBox [bg white]                                              >>= \ b ->
        newSeparator [flexible, parent b]                               >> 
        button [text "Multiply", parent b]                      >>= \ b1 ->
        registerCB el (receive b1 >>> do {
                    bc <- button [text "Clone", parent b];
                    activate bc; 
                    done
                    }
                ) b1                                            >>
        window b [text "Test Packer 1"]                         >>= \win ->
        registerCB el (destroyed win >>> logout) win            >>      
        enterEventLoop el                                       >>
        done
        where   activate :: Button () -> IO ()
                activate bc =
                        newGUIEventLoop                         >>= \el ->
                        registerCB el (receive bc >>> bell) bc >>
                        enterEventLoop el
                logout = do {gui <- getToolInstance; destroy (gui::HTk)}
                

testPacker2 :: IO ()
testPacker2 =
        newGUIEventLoop                                         >>= \el ->
        newCanvas [bg white, flexible]                          >>= \ cv ->
        newOval [position (100,100),size (200,200),parent cv]   >>= \ ov ->
        newRectangle [size (100,100),parent cv]                 >>
        newRectangle [size (90,90),filling blue,outline red,parent cv] >>
        newOval [position (100,100),size (200,200),parent cv]           >>
        newOval [coord [(150,400),(200,500)],
              filling blue, outline red,parent cv]              >>= \ov ->
        registerCB el  (mouseButtonPress ov 1 >>> bell) ov              >> 
        newPolygon [coord [(50,50),(100,100),(200,200)],parent cv]      >>
        newArc [geo (50,50,50,50),extent 90,parent cv]          >>
        newLine [coord [(50,50),(0,100),(300,300)],parent cv]   >>
        newTextItem [pos (200,200),value "howdy",parent cv]     >>
        newBitMapItem [pos (100,200),parent cv]                 >>
        newBitMapItem [
                position (100,200),
                wbbitmap "trash.bm",
                parent cv  {-,
                anchor Center -}
                ]                                               >>
        newImage [filename "~ewk/www/ewk.gif"]                  >>= \ img ->
        newImageItem [ 
                position (200,100),
                photo img,
                parent cv {-,
                anchor Center -}
                ]                                               >>
        button [text "hello", 
                action el (receive @>> bell)]                   >>= \ btn ->
        newEmbeddedCanvasWin btn [pos (200,300),
                anchor West,parent cv]                          >>
        window cv [text "Test Packer 2", size (cm 15,cm 20)]    >>= \ win ->    
        registerCB el (destroyed win)   win             >>
        enterEventLoop el                                       >>
        configure ov [filling blue]                             >>      
        done


testPacker3 :: IO ()
testPacker3 =
        newGUIEventLoop                                         >>= \el ->
        newCanvas [bg white,flexible]                           >>= \ cv ->
        window cv [text "Test Packer 3"]                        >>= \ win ->    
        registerCB el (destroyed win) win                       >>
        newOval [pos (100,100),size (200,200), filling red,parent cv] >>= \ov ->
        registerCB el (mouseButtonPress ov 1 >>> bell) win      >> 
        button [text "hello", 
                action el (receive @>> bell)]                   >>= \ btn ->
        newEmbeddedCanvasWin btn [pos (200,300),anchor West,parent cv] >>
        enterEventLoop el                                               >>      
        done


testPacker4 :: IO ()
testPacker4 =
        newGUIEventLoop                                         >>= \el ->
        newCounter [bg white, fg red, font (Helvetica,Bold,120::Int),
                activeForeground blue, activeBackground green,
                action el (receive @>> done)]                   >>= \cv ->
        window cv [text "Colour Distribution", 
                action el (destroyed @>> done)]                 >>
        enterEventLoop el                                       >>      
        done


-- --------------------------------------------------------------------------
-- Event-Action-Condition
-- --------------------------------------------------------------------------           
testEAC = 
        newGUIEventLoop                                         >>= \el ->
        newCounter [value 10]                                   >>= \ cnt ->
        registerCB el  (receive cnt ? >>= ccond >>> bell) cnt    >>= \ ev' ->
        window cnt [text "EAC"]                                         >>= \win ->
        registerCB el (destroyed win >>> destroy el) win                        >>
        enterEventLoop el                                               >>
        done
        where ccond :: Int -> IO Bool
              ccond x = if (x < 2) && (x > -2) then return True else return False



-- --------------------------------------------------------------------------
-- IMAGES
-- --------------------------------------------------------------------------           

testImage =
        newVBox []                                                      >>= \ vb ->
        newImage [filename "~ewk/www/ewk.gif", parent vb]               >>= \ cb1 ->
        newImage [filename "~ewk/www/room.gif", parent vb]              >>= \ cb2 ->
        newMessage [parent vb]                                  >>= \ msg ->
        value "Press button below to change its image" msg      >>
        button [photo cb1, parent vb]                           >>= \ bt1 -> 
        button [text "Change First Image", parent vb]           >>= \ bt2 -> 
        window vb [text "Test Image"]                           >>= \ win ->
        controller win (control1 bt1 bt2 cb1 cb2)               >>
        runImageTest cb1                                        >>
        runImageTest cb2                                        >>
        done
        where runImageTest cb =
                testBaseWidget cb                                       >>
                testHasColour cb                                        >>
                testHighlightedWidget cb                                >>
                testHasWidth cb                                         >>
                testHasAnchor cb                                        >>
                testHasFile cb                                          >>
                done
              control1 :: Button () -> Button () -> Image -> Image -> InterActor -> EV ()
              control1 bt1 bt2 cb1 cb2 iact =
                     receive bt2 >>> do {
                        filename "~ewk/www/room.gif" cb1; 
                        done
                        }
                 +>  receive bt1 >>> do {
                        configure bt1 [photo cb2]; 
                        become iact (control1 bt1 bt2 cb2 cb1)
                        }



-- --------------------------------------------------------------------------
-- BITMAPS
-- --------------------------------------------------------------------------           
testBitMap =
        newVBox []                                                      >>= \ hb ->
        newBitMap [wbfilename flagup, parent hb]                        >>= \ bm ->
        ((newLabel [bitmap bm, parent hb]) :: IO (Label BitMap))        >>= \ lbl ->
        button [text "Post Mail", parent hb]                    >>= \ b1 ->
        button [text "Fetch Mail",parent hb]                    >>= \ b2 ->
        window hb [text "Test BitMap"]                          >>= \ win ->
        controller' win (events b1 b2 bm)                       >>
        runBitMapTest bm                                        >>
        done
        where runBitMapTest cb = 
                testBaseWidget cb                               >>
                testHasColour cb                                        >>
                testHighlightedWidget cb                        >>
                testHasWidth cb                                 >>
                testHasAnchor cb                                >>
                testHasFile cb                                  >>
                done
              flagup = "toggle_on.bm"
              flagdown = "toggle_off.bm"
              events b1 b2 bm = 
                  receive b1 >>> do {wbfilename flagup bm ; done} 
               +> receive b2 >>> do {wbfilename flagdown bm; done}

-- --------------------------------------------------------------------------
-- EVENT LOOP
-- --------------------------------------------------------------------------           

testEventLoop =
        newGUIEventLoop                                         >>= \ el ->
        newVBox []                                                      >>= \ box ->
        newCounter [parent box]                                 >>= \ cnt ->
        registerCB el (receive cnt >>> done) cnt                        >>
        window box [
                text "Event Loop", 
                action el (destroyed @>> destroy el)]           >>
        enterEventLoop el

        
-- --------------------------------------------------------------------------
-- TEST: Window Configurations
-- --------------------------------------------------------------------------           
testWindow =
        button [text "Hello"]                                   >>= \ cb ->
        window cb [text "Test Win1"]                            >>= \ win ->
        controller' win inaction                                >>
        testWindowConfigs win                                   >>
        button [text "Hello"]                                   >>= \ cb2 ->
        window cb2 [text "Test Win2"]                           >>= \ win2 ->
        putWinOnTop win2                                        >>
        putWinAtBottom win2                                     >>
        ringBell (Just win2)                                    >>
        configure win2 [
                geometry (200,200,600,600),
                maxSize (400,400),
                minSize (50,50),
                positionFrom User,
                sizeFrom User,
                aspectRatio (AspectRatio 1 1 1 1)
                ]                                               >>
        configure (Icon win2) [
                text "Test WIN2",
                bitmap questhead, 
                pos (10,10),
                iconMask question
                ]                                               >>
        testWindowConfigs win2                                  >>
        done

testWindowConfigs :: Window -> IO ()
testWindowConfigs win =
        testHasWidth win                                        >>
        testHasHeight win                                       >>
        fetchC win getText                                      >>
        fetchC (Icon win) getText                               >>      
        fetchC win getMaxSize                                   >>      
        fetchC win getMinSize                                   >>      
        fetchBM (Icon win) getBitMap                            >>
        fetchBM (Icon win) getIconMask                          >>
        fetchC (Icon win) getPosition                           >>
        fetchC win getWindowState                               >>
        fetchC win getSize                                      >>
        fetchC win getPosition                                  >>      
        fetchC win getGeometry                                  >>
        fetchC win getAspectRatio                               >>
        fetchC win getPositionFrom                              >>
        fetchC win getSizeFrom                                  >>
        done


-- --------------------------------------------------------------------------
-- TEST: Remote Window
-- --------------------------------------------------------------------------           
testRemoteWindow disp =
        forkIO(newAlertWin newMessage [modal True]) 
        where newMessage = 
                "Hello Mr. Kohlyang. This is a test of YUM (Yet another User interaction Manager). Sorry to bother you. Please Press button below to get rid of this silly notice."


-- --------------------------------------------------------------------------
-- TEST: Remote Window
-- --------------------------------------------------------------------------           
testTimeout =
        button [text "Click Me Please"]                         >>= \ cb ->
        window cb [text "Timeout"]                              >>= \ win ->
        controller' win (
                receive cb >>> done
          +>    timeout (secs 5) >>> bell
        )


-- --------------------------------------------------------------------------
-- TEST: FOCUS
-- --------------------------------------------------------------------------           
testfocus =
        newGUIEventLoop                                         >>= \ el ->
        newVFBox []                                             >>= \ box ->
        newEntry [value "",parent box]                          >>= \e1 ->
        newEntry [value "",parent box]                          >>= \e2 ->
        newLabel [value "", parent box]                         >>= \lbl ->
        button [text "SetFocus1", parent box,
                action el (receive @>> setFocus e1)]            >>
        button [text "SetFocus2", parent box,
                action el (receive @>> setFocus e2) ]                   >>
        button [text "ForceFocus1",parent box,
                action el (receive @>> forceFocus e1)]          >>
        button [text "ForceFocus2", parent box,
                action el (receive @>> forceFocus e2) ]         >>
        button [text "ShowRecentFocus",parent box]              >>= \b1 ->
        button [text "ShowFocus", parent box]                   >>= \b2 ->
        window box [text "Test Focus", focusModel ActiveFocus,
                action el (destroyed @>> destroy el)]           >>= \ win ->
        registerCB el (receive b1 >>> do {showRecentFocus win lbl; done}) b1 >>
        registerCB el (receive b2 >>> do {showFocus win lbl; done}) b2  >>
        enterEventLoop el                                               >>
        done
        where printFocus lbl ob =       
                case ob of
                        Nothing -> value "Nothing" lbl
                        (Just gid) -> value ((show . objectID) gid) lbl

              showRecentFocus b lbl = getRecentFocus b >>=  printFocus lbl
              showFocus b lbl = getFocus b >>= printFocus lbl



pos :: HasPosition w => Position -> Config w
pos = position

geo :: HasGeometry w => Geometry -> Config w
geo = geometry
                        

white = toColour "white"
blue = toColour "blue"
red = toColour "red"
green = toColour "green"


vwm win ev = destroyed win +> ev


wbbitmap :: HasBitMap w => String -> Config w
wbbitmap fnm w = do {
        prefix <- Posix.getEnvVar "WB_ROOT";
        configure w [bitmap (prefix ++ "/newImages/" ++ fnm)];
        return w
}


wbfilename :: HasFile w => String -> Config w
wbfilename fnm w = do {
        prefix <- Posix.getEnvVar "WB_ROOT";
        configure w [filename (prefix ++ "/newImages/" ++ fnm)];
        return w
}


(@>>) :: ( w -> EV a) -> IO () -> (w -> EV ())
f @>> c = \w -> f w >>> c


action :: GUIObject w => EventLoop GUIOBJECT -> (w -> EV ()) -> Config w
action el f w = do
        el # registerEH w  (f w)
        return w



instance GUIValue [Int] where
        cdefault = []


registerCB el ev o = el # registerEH o ev
