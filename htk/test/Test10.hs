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

import Util
import IO(stdout)
import Debug(debug)

main:: IO ()
main = do
        htk []  
        setLogFile (Just stdout)
        l <- newLabel [value "hello world"]
        window l [text "My first HTk program"] 
        done
