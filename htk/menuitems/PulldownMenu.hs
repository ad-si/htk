{- #########################################################################

MODULE        : PullDownMenu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module PulldownMenu (
        module Menu,
        module Button,
        module MenuButton,
        module RadioButton,
        module CheckButton,

        newPulldownMenu,
        newCascadeMenu,

        newMenuItem,
        newRadioItem,
        newCheckItem
        
        ) where

import Concurrency
import GUICore
import Menu
import MenuButton
import MenuItem
import Button
import CheckButton
import RadioButton
import Debug(debug)

                
-- --------------------------------------------------------------------------
-- Pull Down Menu
-- --------------------------------------------------------------------------   

newPulldownMenu :: MenuButton a -> [Config (Menu a)] -> IO (Menu a)
newPulldownMenu mb confs = do {
        mn <- newMenu confs;
        parent mb mn;
        return mn
        }
        
                
-- --------------------------------------------------------------------------
-- Cascade Menu
-- --------------------------------------------------------------------------   

newCascadeMenu :: [Config (MenuButton a)] -> IO (Menu a)
newCascadeMenu cnfs = do {
        mn <- newMenu [];
        mb <- newMenuButton cnfs;
        parent mb mn;
        return mn
        }

                
-- --------------------------------------------------------------------------
-- Menu items (the Tk way)
-- --------------------------------------------------------------------------   

newMenuItem :: Menu a -> [Config (Button a)] -> IO (Button a)
newMenuItem mn confs = do 
        bt <- newButton confs
        bt # parent mn

newRadioItem :: Menu a -> [Config (RadioButton a)] -> IO (RadioButton a)
newRadioItem mn confs = do
        bt <- newRadioButton confs
        bt # parent mn

newCheckItem :: Menu a -> [Config (CheckButton a)] -> IO (CheckButton a)
newCheckItem mn confs = do
        bt <- newCheckButton confs
        bt # parent mn


