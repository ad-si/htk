{- #########################################################################

MODULE        : Util
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}

module Util where

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
import Debug(debug)



logout :: IO ()
logout = do {gui <- getToolInstance; destroy (gui::HTk)}


-- --------------------------------------------------------------------------
-- TEST: Widget Configurations
-- --------------------------------------------------------------------------           
type TPRC w a = w -> (w -> IO a) ->  IO (Answer ())


testBaseWidget :: (Widget w, HasBorder w) => w -> IO ()
testBaseWidget w = 
        testHasBorder w                                         >>
        testHasCursor w                                         >>
        done

setBaseWidget :: (Widget w, HasBorder w) => Config w    
setBaseWidget w = do {
        setBorder w;
        setCursor w;
        return w
        }

testHasBorder :: HasBorder w => w -> IO ()
testHasBorder w = 
        fetchC w getBorderwidth                                 >>
        fetchC w getRelief                                      >>
        done            

setBorder :: (Widget w, HasBorder w) => Config w        
setBorder w = do {
        borderwidth 1 w;
        relief Sunken w;
        return w
        }

testHasCursor :: Widget w => w -> IO ()
testHasCursor w = 
        fetchC w getCursor                                      >>
        fetchC w getTakeFocus                                   >>
        done
                        
setCursor :: Widget w => Config w       
setCursor w = do {
        cursor "circle" w;
        return w
        }


testHasColour :: HasColour w => w -> IO ()
testHasColour w = 
        fetchC w getBackground                                  >>
        fetchC w getForeground                                  >>
        fetchC w getActiveBackground                            >>
        fetchC w getActiveForeground                            >>
        fetchC w getDisabledForeground                          >>
        done

setWidgetColour :: HasColour w => Config w
setWidgetColour w = configure w [
        bg "red", 
        fg "white", 
        activeBackground "blue",
        activeForeground "green",
        disabledForeground "grey"
        ]



testHighlightedWidget :: Widget w => w -> IO ()
testHighlightedWidget w = 
        fetchC w getHighlightBackground                         >>
        fetchC w getHighlightColour                             >>
        fetchC w getHighlightThickness                          >>
        done

setHighlightedWidget :: (Widget w,HasColour w) => Config w              
setHighlightedWidget w =
        configure w [
                highlightBackground "red",
                highlightColour "yellow",
                highlightThickness 1
                ]

testHasWidth :: HasSize w => w -> IO ()
testHasWidth w = 
        fetchC w getWidth                                       >>
        done


testHasHeight :: HasSize w => w -> IO ()
testHasHeight w = 
        fetchC w getHeight                                      >>
        fetchC w getSize                                                >>
        done

testHasColumns :: HasSize w => w -> IO ()
testHasColumns w = 
        fetchC w getWidth                                               >>
        done


testHasRows :: HasSize w => w -> IO ()
testHasRows w = 
        fetchC w getHeight                                      >>
        done

setSize :: HasSize w => Config w
setSize w = configure w [height 10, width 10]

testHasPosition :: HasPosition w => w -> IO ()
testHasPosition w = 
        fetchC w getPosition                                    >>
        done

testHasGeometry :: HasGeometry w => w -> IO ()
testHasGeometry w = 
        fetchC w getGeometry                                    >>
        done


testHasFont :: HasFont w => w -> IO ()
testHasFont w = 
        fetchC w getFont                                                >>
        done

setFont :: HasFont w => Config w
setFont w = configure w [font (Helvetica,10::Int)]

testHasText :: HasText w String => w -> IO ()
testHasText w = 
        fetchC w getT                                           >>
        done
         where getT :: HasText w String => w -> IO String
               getT = getText

testHasValue :: (GUIValue a,Variable w a) => w a -> IO a
testHasValue w = do
        v <- getValue w
        print v
        return v

testHasElements :: (GUIValue a,Variable w [a]) => w [a] -> IO [a]
testHasElements w = do
        v <- getValue w
        print v
        return v



testTextStateWidget :: Variable w [Char] => w [Char] -> IO ()
testTextStateWidget w = 
        updValue w (++ "<S>")                                   >>
        updValue w ("<P>" ++)                                   >>
        done

testHasUnderline :: HasUnderline w => w -> IO ()
testHasUnderline w = 
        fetchC w getUnderline                                   >>
        fetchC w getWraplength                                  >>
        done

setUnderline :: HasUnderline w => Config w
setUnderline w = configure w [underline 1, wraplength 1]


testHasAnchor :: Widget w => w -> IO ()
testHasAnchor w = 
        fetchC w getAnchor                                      >>
        done

testSymbolicWidget :: (HasBitMap w, HasPhoto w) => w -> IO ()
testSymbolicWidget w = 
        testHasBitMap w                                         >>
        testHasPhoto w                                          >>
        done

setSymbolicWidget  :: (HasBitMap w, HasPhoto w) => Config w 
setSymbolicWidget w = do {
        img <- newImage [filename "~ewk/www/ewk.gif"];  
        configure w [bitmap questhead, photo img]
        }
 

testHasBitMap :: HasBitMap w => w -> IO ()
testHasBitMap w = 
        fetchBM w getBitMap                                             >>
        done


testHasPhoto :: HasPhoto w => w -> IO ()
testHasPhoto w = 
        getPhoto w                                              >>
        done


testHasJustify :: HasJustify w => w -> IO ()
testHasJustify w = 
        fetchC w getJustify                                     >>
        done

setJustify ::  HasJustify w => Config w 
setJustify w = configure w [justify JustCenter]

testHasEnable :: HasEnable w => w -> IO ()
testHasEnable w = 
        fetchC w getState                                       >>
        done

setEnable ::  HasEnable w => Config w 
setEnable w = configure w [state Disabled]



testSelection :: (HasSelection w, Widget w) => w -> IO ()
testSelection w = 
        fetchC (Selection w) getBackground                      >>
        fetchC (Selection w) getBorderwidth                     >>
        fetchC (Selection w) getForeground                      >>
        done


setSelection :: (Widget w, HasSelection w) => Config w
setSelection w = do {
        configure (Selection w) [
                bg "blue",
                borderwidth 1,
                fg "green"
                ];
        return w
        }

testHasICursor :: HasInsertionCursor w => w -> IO ()
testHasICursor w = 
        fetchC (ICursor w) getBackground                                >>
        fetchC (ICursor w) getBorderwidth                       >>
        fetchC (ICursor w) getWidth                             >>
        fetchC (ICursor w) getInsertOffTime                     >>
        fetchC (ICursor w) getInsertOnTime                      >>
        done


setICursor :: HasInsertionCursor w => Config w
setICursor w = do {
        configure (ICursor w) [
                bg "red",
                borderwidth 1,
                width 1,
                insertOffTime 1,
                insertOnTime 1
                ];
        return w
        }


testHasSlider :: HasSlider w => w -> IO ()
testHasSlider w = 
        fetchC (Slider w) getBackground                         >>
        fetchC (Slider w) getForeground                         >>
        fetchC (Slider w) getRepeatInterval                     >>
        fetchC (Slider w) getRepeatDelay                        >>
        done


setSlider ::  HasSlider w => Config w 
setSlider w = do {
        configure (Slider w) [
                bg "red",
                fg "blue",
                repeatInterval 1,
                repeatDelay 1
                ];
        return w
        }


testHasOrientation :: HasOrientation w => w -> IO ()
testHasOrientation w = 
        fetchC w getOrient                                      >>
        done


testHasGrid :: HasGrid w => w -> IO ()
testHasGrid w = 
        fetchC w getGrid                                                >>
        done

testHasFile :: HasFile w => w -> IO ()
testHasFile w = 
        fetchC w getFileName                                    >>
        done





testHasBBox :: HasBBox w i => w -> i -> IO ()
testHasBBox w i = 
--      bbox w i                                                >>
        done



testMultipleLinedWidget :: (HasLineSpacing w,HasTabulators w) => w -> IO ()
testMultipleLinedWidget w = 
        fetchC w getSpaceAbove                                  >>
        fetchC w getSpaceWrap                                   >>
        fetchC w getSpaceBelow                                  >>
        done


testHasAccelerator :: HasAccelerator w => w -> IO ()
testHasAccelerator w =
        fetchC w getAccelerator                                 >>
        done

testHasIndicator :: HasIndicator w => w -> IO ()
testHasIndicator w =
        fetchC w getIndicator                                   >>
        done



testHasInsertionCursor :: (Eq i, HasInsertionCursorIndexSet w i,
        HasInsertionCursorIndexGet w i) => w -> i -> IO ()
testHasInsertionCursor w i =
        configure w [insertionCursor i]                         >>
        getInsertionCursor w                                    >>= \i' ->
        unless (i == i') done                                   >>
        done


testHasSelection w i1 i2 = done

{-
testHasSelection :: (Index i1, Index i2,HasSelection w, HasSelectionIndexRange w i1 i2) => w -> i1 -> i2 -> IO ()
testHasSelection w i1 i2 =
        configure w [selection i1]                              >>
        runTestSel                                              >>
        configure w [selectionRange i1 i2]                      >>
        runTestSel                                              >>
        done
        where runTestSel =
                getSelection w                                  >>
                mfetchC w getSelectionStart                     >>
                getSelectionEnd w                               >>
--              getSelectionState w                             >>
                isSelected w i1                                 >>
                isSelected w i2                                 >>
                clearSelection w                                >>
                done
-}

testPrintable :: HasPostscript w => w -> String -> IO ()
testPrintable w fname =
        postscript w [                                  
                filename fname,
                colourmode FullColourMode,
                pageAnchor Center,
                pageheight (cm 10),
                pagewidth (cm 10),
                pagex (cm 3),
                pagey (cm 3),
                rotate True
                ]                                               >>
        done



-- --------------------------------------------------------------------------
-- MENU
-- --------------------------------------------------------------------------           

testSeparatorWidget :: Separator -> IO ()
testSeparatorWidget sp =
        done


testMenuWidget :: Menu a -> IO ()
testMenuWidget mn =
        testBaseWidget mn                                       >>
        testHasColour mn                                                >>
        testHasFont mn                                          >>
        post mn (cm 10,cm 10)                                   >>
        unpost mn                                               >>
        done
        
testMenuButtonWidget :: MenuButton a -> IO ()
testMenuButtonWidget cb =
        testHasAccelerator cb                                   >>
        testHasIndicator cb                                     >>
        testBaseWidget  cb                                      >>
        testHasColour cb                                                >>
        testHasWidth cb                                         >>
        testHasHeight cb                                        >>
        testHasFont cb                                          >>
        testHasText cb                                          >>
        testHasUnderline cb                                     >>
        testHasAnchor cb                                        >>
        testSymbolicWidget cb                                   >>
        testHasJustify cb                                       >>
        done

testButtonWidget :: (Widget w, HasColour w, HasSize w,
        HasFont w, HasText w String, HasUnderline w, HasJustify w,
        HasBorder w, HasBitMap w, HasPhoto w, ButtonWidget w) =>
                w -> IO ()
testButtonWidget cb =
        testBaseWidget  cb                                      >>
        testHasColour cb                                                >>
        testHighlightedWidget   cb                              >>
        testHasWidth cb                                         >>
        testHasHeight cb                                        >>
        testHasFont cb                                          >>
        testHasText cb                                          >>
        testHasUnderline cb                                     >>
        testHasAnchor cb                                        >>
        testSymbolicWidget cb                                   >>
        testHasJustify cb                                       >>
        flash cb                                                >>
        invoke cb                                               >>
        done

setButtonWidget :: (Widget w, HasColour w, HasSize w,
        HasFont w, HasText w String, HasUnderline w, HasJustify w,
        HasBorder w, HasBitMap w, HasPhoto w, ButtonWidget w) =>
                Config w
setButtonWidget cb =
        setBaseWidget   cb                                      >>
        setWidgetColour cb                                      >>
        setHighlightedWidget    cb                              >>
        setSize cb                                              >>
        setFont cb                                              >>
        setUnderline cb                                         >>
        setSymbolicWidget cb                                    >>
        setJustify cb                                           >>
        flash cb                                                >>
        invoke cb                                               >>
        return cb



testSelectButtonWidget cb =
        testButtonWidget cb                                     >>
        fetchC (Indicator cb) getBackground                     >>
        (selectionState On cb >> done)                          >>
        (selectionState Off cb >> done)                         >>
        done


-- --------------------------------------------------------------------------
-- Canvas Item
-- --------------------------------------------------------------------------           
testCanvasItem cv =
        testBaseWidget cv                                       >>
        testHasColour cv                                                >>
        testHasWidth cv                                         >>
        testHasHeight cv                                        >>
        testHighlightedWidget cv                                >>
--      testSelection cv                                        >>      
--      testHasICursor cv                                       >>
        fetchC cv getCloseEnough                                        >>
        fetchC cv getConfine                                    >>
        done

testRectangleItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
        testFilledCanvasItem cir                                >>
        testHasGeometry cir                                     >>
        testHasWidth cir                                        >>
        testHasHeight cir                                       >>
        testHasPosition cir                                     >>
        done

testOvalItem :: Oval -> IO ()
testOvalItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
        testFilledCanvasItem cir                                >>
        done

testPolygonItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
        testFilledCanvasItem cir                                >>
        testSegmentedCanvasItem cir                             >>
        done

testArcItem cir =
        testBaseCanvasItem      cir                             >>
        testTaggedCanvasItem cir                                >>
        testFilledCanvasItem cir                                >>
        testHasPosition cir                                     >>
        fetchC cir getExtent                                    >>
        fetchC cir getStart                                     >>
        done

testLineItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
        testFilledCanvasItem cir                                >>
        testSegmentedCanvasItem cir                             >>
        fetchC cir getArrowstyle                                        >>
        fetchC cir (\w -> getArrowshape w >>= wrap)             >>
        fetchC cir getCapstyle                                  >>
        fetchC cir getJoinstyle                                 >>
        done
        where wrap (x,y,z) = return [x,y,z]

testBitMapItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
--      testHasAnchor cir                                       >>
        testHasBitMap cir                                       >>
        testFilledCanvasItem cir                                >>
        testHasPosition cir                                     >>
        done

testImageItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
--      testHasAnchor cir                                       >>
        testHasPhoto cir                                        >>
        testHasPosition cir                                     >>
        done

testTextItem cir =
        testBaseCanvasItem cir                                  >>
        testTaggedCanvasItem cir                                >>
--      testHasAnchor cir                                       >>
        testHasJustify cir                                      >>
        testHasFont cir                                         >>
        testHasValue cir                                        >>
        testFilledCanvasItem cir                                >>
        testHasPosition cir                                     >>
        done


testBaseCanvasItem ci =
        fetchC ci getCoord                                      >>
        done

testTaggedCanvasItem :: TaggedCanvasItem w => w -> IO ()
testTaggedCanvasItem w = 
-- TBD
        done            


testFilledCanvasItem :: FilledCanvasItem w => w -> IO ()
testFilledCanvasItem w = 
        fetchC w getFilling                                     >>
        fetchC w getOutline                                     >>
        getStipple w                                            >>
        fetchC w getOutlineWidth                                >>
        done            

        
testSegmentedCanvasItem :: SegmentedCanvasItem w => w -> IO ()
testSegmentedCanvasItem w = 
        fetchC w getSplinesteps                                 >>
        fetchC w getSmooth                                      >>
        done            


        
type MTPRC w a = w -> (w -> IO (Maybe a)) ->  IO (Answer ())

fetchC :: GUIValue a => TPRC w a
fetchC cb cmd = 
        try (cmd cb >>= showres)

mfetchC :: GUIValue a => MTPRC w a
mfetchC cb cmd = 
        try (cmd cb >>= mshowres)
        where mshowres Nothing = done
              mshowres (Just val) = showres val


fetchBM :: w -> (w -> IO BitMapHandle) -> IO ()
fetchBM w get = do {
        bm <- get w;
        case bm of 
                (Predefined s) -> print s
                (BitMapFile s) -> print s
                (BitMapHandle b) -> print "handle"
        }

showres val = print val

