{- --------------------------------------------------------------------
 -
 - GenGUI example
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}

module Main (main) where

import HTk
import Button
import Entry
import Image
import Property
import GenGUI
import RVar


------------------
-- number items --
------------------

data NumItem = NumFolderItem (Prop Name) (Prop ItemIcon)
             | NumItem (RVar Int) (Prop Name) (Prop ItemIcon)

instance HasProp NumItem Name where
  getProp (NumFolderItem p _) = p
  getProp (NumItem _ p _) = p

instance HasProp NumItem ItemIcon where
  getProp (NumFolderItem _ p) = p
  getProp (NumItem _ _ p) = p


----------------
-- text items --
----------------

data TxtItem = TxtFolderItem (Prop Name) (Prop ItemIcon)
             | TxtItem (RVar String) (Prop Name) (Prop ItemIcon)

instance HasProp TxtItem Name where
  getProp (TxtFolderItem p _) = p
  getProp (TxtItem _ p _) = p

instance HasProp TxtItem ItemIcon where
  getProp (TxtFolderItem _ p) = p
  getProp (TxtItem _ _ p) = p


-------------------------
-- add folders / items --
-------------------------

addNum :: GenGUI -> String -> IO ()
addNum gui nm =
  do
    putStrLn ("adding num folder: name = '" ++ nm)

addTxt :: GenGUI -> String -> IO ()
addTxt gui nm =
  do
    putStrLn ("adding txt folder: name = '" ++ nm)
    pname <- newProp {fullname  = nm,
                      shortname = \n -> take n nm}
    picon <- newProp folderImg
    val1 <- newRVar ("content of '" ++ nm ++ "1'")
    pname1 <- newProp {fullname  = nm ++ "1",
                       shortname = \n -> take n (nm ++ "1")}
    picon1 <- newProp folderImg
    val2 <- newRVar ("content of '" ++ nm ++ "2'")
    pname2 <- newProp {fullname  = nm ++ "2",
                       shortname = \n -> take n (nm ++ "2")}
    picon2 <- newProp folderImg
    val3 <- newRVar ("content of '" ++ nm ++ "3'")
    pname3 <- newProp {fullname  = nm ++ "3",
                       shortname = \n -> take n (nm ++ "3")}
    picon3 <- newProp folderImg
    guiroot <- root gui
    addItem guiroot (FolderItem (TxtFolderItem pname picon)
                                [LeafItem (TxtItem val1 pname1 picon1),
                                 LeafItem (TxtItem val2 pname2 picon2),
                                 LeafItem (TxtItem val3 pname3 picon3)])

----------
-- init --
----------

main :: IO ()
main =
  do
    idref <- newRVar 0
    tk <- htk []
    main <- newVBox []
    win <- window main [text "GenGUI eample"]
    gui <- newGenGUI
    boxtxt <- newHBox [parent main]
    addtxt <- newButton [pad Vertical 5, pad Horizontal 5,
                         text "add text item folder", width 40, parent boxtxt,
                         command (\ () -> return ())]
    txtnm <- newEntry [pad Vertical 5, pad Horizontal 5, width 20,
                       background "white", parent boxtxt]
    boxnum <- newHBox [parent main]
    addnum <- newButton [pad Vertical 5, pad Horizontal 5,
                         text "add number folder", width 40, parent boxnum,
                         command (\ () -> return ())]
    numnm <- newEntry [pad Vertical 5, pad Horizontal 5, width 20,
                       background "white", parent boxnum]
    interactor (\i -> (triggered addtxt >>> do
                                              nm <- getValue txtnm
                                              addTxt gui nm) +>
                      (triggered addnum >>> do
                                              nm <- getValue numnm
                                              addNum gui nm))
    sync (destroyed win)
    destroy tk


-- temp --

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]
