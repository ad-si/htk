-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Main (main) where

import HTk
import GenGUI
import Name
import ReferenceVariables
import System
import IOExts
import Dynamic

foldref :: Ref (Maybe (Item Obj))
foldref = unsafePerformIO (newRef Nothing)

imgpathref :: Ref (Maybe FilePath)
imgpathref =
  unsafePerformIO (newRef Nothing)

lastactiveref :: Ref (Button ())
lastactiveref = unsafePerformIO (newRef (unsafePerformIO
                                           (newButton NONE [])))

imgref :: Ref (IO Image)
imgref = unsafePerformIO (newRef folderImg)

data Col = Red | Green | Blue | Yellow deriving Eq

type Obj = (Name, IO Image, MyObject)

type Id = Int

data MyObject =
    MyContainer Id
  | MyColor Id Col
  | MyImage Id (IO Image)

getID :: MyObject -> Id
getID (MyContainer id) = id
getID (MyColor id _) = id
getID (MyImage id _) = id

instance Eq MyObject where
  myobj1 == myobj2 = getID myobj1 == getID myobj2

instance CItem Obj where
  getIcon (_, ic, _) = ic
  getName (nm, _, _) = return nm

instance Eq Obj where
  (_, _, myobj1) == (_, _, myobj2) = myobj1 == myobj2


----------------------------------------
-- external adding of folders / items --
----------------------------------------

{-
addNum :: String -> String -> IO ()
addNum nm ent = putStrLn ("not yet implemented")
-}

{-
addTxt :: String -> String -> IO ()
addTxt nm ent =
  do
    mpar <- getRef foldref
    case mpar of
      Just par -> do
                    pname <- newProp (newName nm)
                    picon <- newProp txtImg
                    pval <- newProp (toDyn ent)
                    addItem par (LeafItem (MyObject pname picon pval))
                    done
      _ -> done
-}

addCol :: IO Id -> GenGUI Obj -> String -> String -> IO ()
addCol newID gui name ent =
  do
    mpar <- getRef foldref 
    case mpar of
      Just par -> 
        do
          id <- newID
          let nm = newName name
              ic = case ent of "Red" -> redImg
                               "Blue" -> blueImg
                               "Green" -> greenImg
                               "Yellow" -> yellowImg
              val = MyColor id (case ent of "Red" -> Red
                                            "Blue" -> Blue
                                            "Green" -> Green
                                            "Yellow" -> Yellow)
          addItem gui par (LeafItem (nm, ic, val) Nothing)
          done
      _ -> done

{-
addImg :: String -> Maybe FilePath -> IO ()
addImg nm mimgpath =
  do
    mpar <- getRef foldref
    case mpar of
      Just par ->
        case mimgpath of
          Just imgpath -> do
                            let img = newImage NONE [filename imgpath]
                            pval <- newProp (toDyn img)
                            pname <- newProp (newName nm)
                            picon <- newProp imgImg
                            addItem par
                                    (LeafItem (MyObject pname picon pval))
                            done
          _ -> done
      _ -> done
-}

{-
addFolder :: String -> IO ()
addFolder nm =
  do
    mpar <- getRef foldref
    case mpar of
      Just par ->
        do
          pval <- newProp (toDyn nm)
          pname <- newProp (newName nm)
          img <- getRef imgref
          picon <- newProp img
          addItem par (FolderItem (MyContainer pname picon pval) [])
          done
      _ -> done
-}


-------------------
-- example items --
-------------------

addExampleFolders :: IO Id -> GenGUI Obj -> IO ()
addExampleFolders newID gui =
  let 
{-
      mkTxtItem :: String -> (Int, (String, ItemIcon)) -> IO NewItem
      mkTxtItem nm (i, (val, icon)) =
        do
          pname <- newProp (newName (nm ++ show i))
          picon <- newProp icon
          pval <- newProp (toDyn val)
          return (LeafItem (MyObject pname picon pval))

      addTxtFolder :: Item -> String -> ItemIcon -> String ->
                     [(String, ItemIcon)] -> Int -> IO ()
      addTxtFolder par nm icon subnm vals_icons i =
        do
          pname <- newProp (newName (nm ++ show i))
          picon <- newProp icon
          pval <- newProp (toDyn ())
          items <- mapM (mkTxtItem subnm) (zip [1..(length vals_icons)]
                                               vals_icons)
          addItem par (FolderItem (MyContainer pname picon pval) items)
          done
-}
{-
      mkNumItem :: String -> (Int, (Int, ItemIcon)) -> IO NewItem
      mkNumItem nm (i, (val, icon)) =
        do
          pname <- newProp (newName (nm ++ show i))
          picon <- newProp icon
          pval <- newProp (toDyn val)
          return (LeafItem (MyObject pname picon pval))

      addNumFolder :: Item -> String -> ItemIcon -> String ->
                      [(Int, ItemIcon)] -> Int -> IO ()
      addNumFolder par nm icon subnm vals_icons i =
        do
          pname <- newProp (newName (nm ++ show i))
          picon <- newProp icon
          pval <- newProp (toDyn ())
          items <- mapM (mkNumItem subnm) (zip [1..(length vals_icons)]
                                               vals_icons)
          addItem par (FolderItem (MyContainer pname picon pval) items)
          done
-}
      mkImgItem :: IO Id -> String -> (Int, (IO Image, IO Image)) ->
                   IO (NewItem Obj)
      mkImgItem newID name (i, (img, ic)) =
        do
          id <- newID
          let val = MyImage id img
              nm = newName (name ++ show i)
          return (LeafItem (nm, ic, val) Nothing)

      addImgFolder :: IO Id -> GenGUI Obj -> Item Obj -> String ->
                      IO Image -> String -> [(IO Image, IO Image)] ->
                      Int -> IO ()
      addImgFolder newID gui par name ic subnm vals_icons i =
        do
          let nm = newName (name ++ show i)
          items <- mapM (mkImgItem newID subnm)
                        (zip [1..(length vals_icons)] vals_icons)
          id <- newID
          addItem gui par (FolderItem (nm, ic, MyContainer id) items
                                      Nothing)
          done

      mkColItem :: IO Id -> String -> (Int, (Col, IO Image)) ->
                   IO (NewItem Obj)
      mkColItem newID name (i, (col, ic)) =
        do
          id <- newID
          let val = MyColor id col
              nm = newName (name ++ show i)
          return (LeafItem (nm, ic, val) Nothing)

      addColFolder :: IO Id -> GenGUI Obj -> Item Obj -> String ->
                      IO Image -> String -> [(Col, IO Image)] -> Int ->
                      IO ()
      addColFolder newID gui par name ic subnm vals_icons i =
        do
          let nm = newName (name ++ show i)
          items <- mapM (mkColItem newID subnm)
                        (zip [1..(length vals_icons)] vals_icons)
          id <- newID
          addItem gui par (FolderItem (nm, ic, MyContainer id) items
                                      Nothing)
          done
  in do
       guiroot <- root gui

       mapM (addImgFolder newID gui guiroot "images." imgfolderImg
                          "image_item."
                          [(img1, imgImg), (img2, imgImg)]) [1]

       let nm1 = newName ("example_folder.1")
       exfolder1 <- do
                      id <- newID
                      addItem gui guiroot
                        (FolderItem (nm1, folderImg, MyContainer id) []
                                    Nothing)

{-
       mapM (addTxtFolder guiroot "texts." txtfolderImg "text_item"
                          [("content of text item 1", txtImg),
                           ("content of text item 2", txtImg),
                           ("content of text item 3", txtImg),
                           ("content of text item 4", txtImg),
                           ("content of text item 5", txtImg),
                           ("content of text item 6", txtImg),
                           ("content of text item 7", txtImg),
                           ("content of text item 8", txtImg)]) [1..2]
       mapM (addTxtFolder guiroot "texts." txtfolderImg
                          "text_item_width_long_name"
                          [("content of text item 1", txtImg),
                           ("content of text item 2", txtImg),
                           ("content of text item 3", txtImg),
                           ("content of text item 4", txtImg),
                           ("content of text item 5", txtImg),
                           ("content of text item 6", txtImg),
                           ("content of text item 7", txtImg),
                           ("content of text item 8", txtImg)]) [1..2]
       mapM (addNumFolder exfolder1 "numbers." numfolderImg "number_item."
                         [(25 :: Int, numImg), (17, numImg), (8, numImg), 
                          (73, numImg), (2451, numImg), (3, numImg),
                          (7182812, numImg), (2, numImg)]) [1..3]
-}
       mapM (addColFolder newID gui guiroot "colors." colorfolderImg
                          "color_item."
                          [(Red, redImg), (Yellow, yellowImg),
                           (Green, greenImg), (Yellow, yellowImg),
                           (Yellow, yellowImg), (Red, redImg),
                           (Green, greenImg), (Blue, blueImg),
                           (Blue, blueImg)]) [1..3]
       done

{-
chooseImageFile :: Button () -> IO ()
chooseImageFile b =
  do
    homedir <- getEnv "HOME"
    fd <- fileDialog "Open file" homedir
    interactor (\i -> fileChosen fd >>>= \mfp -> case mfp of
                                                   Just fp ->
                                                     b # text(short fp) >>
                                                     setRef imgpathref
                                                            (Just fp)
                                                   _ -> done)
  where short :: String -> String
        short str =
          if length str > 20 then (".."  ++ drop (length str - 18) str)
          else str
-}


----------
-- init --
----------

main :: IO ()
main =
  do
    idref <- newRef 0

    let newID :: IO Id
        newID = do
                  id <- getRef idref
                  setRef idref (id + 1)
                  return id

    main <- initHTk [text "GenGUI example"]

    -- construct gui
    gui <- newGenGUI Nothing :: IO (GenGUI Obj)

    -- create menu content
    m <- createMenu main True []
    pulldown1 <- createMenuCascade (genGUIMainMenu gui)
                                   [text "Info", menu m]
    createMenuCommand m [text "GenGUI Info"]
    createMenuSeparator m []
    createMenuCommand m [text "Quit"]

    top <- newVFBox main []
    pack top [PadX 5, PadY 5, Fill X, Expand On]

    l <- newLabel top [text "add items to selected folder:",
                       font (Helvetica, 12 :: Int), bg "green"]
    pack l [PadX 100, Fill X, Expand On]

    foldlab <- newLabel top [text "no folder selected", relief Sunken,
                             fg "blue", bg "green"]
    pack foldlab [PadX 100, Fill X, Expand On]

{-
    boximg <- newHFBox main []
    pack boximg [PadX 10, PadY 10]

    addimg <- newButton boximg [pad Vertical 5, pad Horizontal 5,
                                height 3, text "add image item", width 28]
                :: IO (Button String)
    pack addimg

    imgentries <- newVBox boximg []
    pack imgentries []

    imgnmbox <- newHBox []
    pack imgnmbox []

    l <- newLabel [text "name:", font (Helvetica, 12::Int), width 8]
    pack l []

    imgnm <- newEntry [pad Vertical 5, pad Horizontal 5, width 30,
                       background "white", parent imgnmbox,
                       text "(default)"] :: IO (Entry String)
    imgvalbox <- newHBox [parent imgentries]
    imgbutton <- newButton [text "choose image", width 30,
                            parent imgvalbox, command (\ () -> return ())]
                   :: IO (Button String)
-}
{-
    boxtxt <- newFrame main []
    pack boxtxt [PadX 10, PadY 10, Fill X, Expand On]
    addtxt <- newButton boxtxt [height 3, text "add text item", width 28]
                :: IO (Button String)
    pack addtxt [PadX 5, PadY 5, Side AtLeft]

    txtentries <- newFrame boxtxt []
    pack txtentries [Side AtRight]
    txtnmbox <- newFrame txtentries []
    pack txtnmbox []
    lab <- newLabel txtnmbox [text "name:", font (Helvetica, 12::Int),
                              width 8]
    pack lab [Side AtLeft]
    txtnm_var <- createTkVariable "(default)"
    txtnm <- newEntry txtnmbox [width 30, background "white",
                                variable txtnm_var] :: IO (Entry String)
    pack txtnm [PadX 5, PadY 5, Side AtRight]

    txtvalbox <- newFrame txtentries []
    pack txtvalbox []
    lab <- newLabel txtvalbox [text "content:",
                               font (Helvetica, 12 :: Int), width 8]
    pack lab [Side AtLeft]
    txtval_var <- createTkVariable ""
    txtval <- newEntry txtvalbox [width 30, variable txtval_var,
                                  background "white"] :: IO (Entry String)
    pack txtval [PadX 5, PadY 5, Side AtRight]
    clickedaddtxt <- clicked addtxt
    spawnEvent (forever (clickedaddtxt >>
                         always (do
                                   nm <- readTkVariable txtnm_var
                                   val <- readTkVariable txtval_var
                                   addTxt nm val
                                   done)))
-}
{-
    boxnum <- newHFBox [pad Vertical 10, pad Horizontal 10, parent main]
    addnum <- newButton [pad Vertical 5, pad Horizontal 5, height 3,
                         text "add number item", width 28, parent boxnum,
                         command (\ () -> return ())]
                :: IO (Button String)
    numentries <- newVBox [parent boxnum]
    numnmbox <- newHBox [parent numentries]
    newLabel [text "name:", font (Helvetica, 12 :: Int), width 8,
              parent numnmbox]
    numnm <- newEntry [pad Vertical 5, pad Horizontal 5, width 30,
                       background "white", parent numnmbox,
                       text "(default)"]
    numvalbox <- newHBox [parent numentries]
    newLabel [text "value:", font (Helvetica, 12 :: Int), width 8,
              parent numvalbox]
    numval <- newEntry [pad Vertical 5, pad Horizontal 5, width 30,
                        background "white", parent numvalbox] :: IO (Entry String)
-}

    boxcol <- newFrame main []
    pack boxcol [PadX 10, PadY 10, Fill X, Expand On]
    addcol <- newButton boxcol [height 3, text "add color item", width 28]
                :: IO (Button String)
    pack addcol [PadX 10, PadY 10, Side AtLeft]
    colentries <- newFrame boxcol []
    pack colentries [Side AtRight]
    colnmbox <- newFrame colentries []
    pack colnmbox []
    lab <- newLabel colnmbox [text "name:", font (Helvetica, 12 :: Int),
                              width 8]
    pack lab [Side AtLeft]
    colnm_var <- createTkVariable "(default)"
    colnm <- newEntry colnmbox [width 30, background "white",
                                variable colnm_var] :: IO (Entry String)
    pack colnm [PadX 5, PadY 5, Side AtRight]
    colmenu <- newOptionMenu colentries ["Red", "Green", "Blue", "Yellow"]
                                        [width 20]
    pack colmenu []
    clickedaddcol <- clicked addcol
    spawnEvent (forever (clickedaddcol >>
                         always (do
                                   nm <- readTkVariable colnm_var
                                   val <- getValue colmenu
                                   addCol newID gui nm val)))
{-
    boxfold <- newHFBox [pad Vertical 10, pad Horizontal 10, parent main]
    addfold <- newButton [pad Vertical 5, pad Horizontal 5, height 3,
                          text "add folder", width 28,
                          parent boxfold,
                          command (\ () -> return ())]
                :: IO (Button String)
    foldentries <- newVBox [parent boxfold]
    foldnmbox <- newHBox [parent foldentries]
    newLabel [text "name:", font (Helvetica, 12 :: Int), width 8,
              parent foldnmbox]
    foldnm <- newEntry [pad Vertical 5, pad Horizontal 5, width 30,
                        background "white", parent foldnmbox,
                        text "(default)"] :: IO (Entry String)
    foldimgbox <- newHBox [parent foldentries]
    folderImg' <- folderImg
    standardfoldimg <- newButton [size (18, 18),
                                  pad Horizontal 3, photo folderImg',
                                  parent foldimgbox,
                                  command (\ () -> return ())]
                         :: IO (Button String)
    setRef lastactiveref standardfoldimg
    imgfolderImg' <- imgfolderImg
    imgfoldimg <- newButton [size (18, 18),
                             pad Horizontal 3, relief Sunken,
                             photo imgfolderImg', parent foldimgbox,
                             command (\ () -> return ())]
                    :: IO (Button String)
    txtfolderImg' <- txtfolderImg
    txtfoldimg <- newButton [size (18, 18),
                             pad Horizontal 3, relief Sunken,
                             photo txtfolderImg', parent foldimgbox,
                             command (\ () -> return ())]
                    :: IO (Button String)
    colorfolderImg' <- colorfolderImg
    colfoldimg <- newButton [size (18, 18),
                             pad Horizontal 3, relief Sunken,
                             photo colorfolderImg', parent foldimgbox,
                             command (\ () -> return ())]
                    :: IO (Button String)
    numfolderImg' <- numfolderImg
    numfoldimg <- newButton [size (18, 18),
                             pad Horizontal 3, relief Sunken,
                             photo numfolderImg', parent foldimgbox,
                             command (\ () -> return ())]
                    :: IO (Button String)
-}
    export <- newButton main [text "Export state"] :: IO (Button String)
    pack export [PadX 10, PadY 5, Fill X, Expand On]
    clickedexport <- clicked export
    quit <- newButton main [text "Quit"]  :: IO (Button String)
    pack quit [PadX 10, PadY 5, Fill X, Expand On]
    clickedquit <- clicked quit


    (gui_ev, _) <- bindGenGUIEv gui

    let --react :: GenGUIEvent c -> IO ()
        react (SelectTreeList mitem) = selectedTl foldlab mitem
        react (Doubleclick item) = doubleClickNp item
        react _ = done

    spawnEvent (forever (clickedexport >>> exportState gui +>
                         clickedquit >>> destroy main +>
                         gui_ev >>>= react))
{-
                         selectedItemInTreeList gui >>>=
                           selectedTl foldlab +>
                         doubleClickInNotepad gui >>>= doubleClickNp))
-}

{-
    interactor (\i -> (triggered addimg >>> do
                                              nm <- getText imgnm
                                              val <- getRef imgpathref
                                              addImg nm val) +>
                      (triggered addtxt >>> do
                                              nm <- getText txtnm
                                              val <- getText txtval
                                              addTxt nm val
                                              done) +>
                      (triggered addnum >>> do
                                              nm <- getText numnm
                                              --val <- getText numval
                                              {-addNum nm val-}
                                              done) +>
                      (triggered addcol >>> do
                                              nm <- getText colnm
                                              val <- getText colmenu
                                              addCol nm val
                                              done) +>
                      (triggered imgbutton >>> chooseImageFile
                                                 imgbutton) +>
                      (triggered addfold >>> do
                                               nm <- getText foldnm
                                               addFolder nm
                                               done) +>
                      (triggered standardfoldimg >>>
                         imgSelected standardfoldimg folderImg) +>
                      (triggered imgfoldimg >>>
                         imgSelected imgfoldimg imgfolderImg) +>
                      (triggered txtfoldimg >>>
                         imgSelected txtfoldimg txtfolderImg) +>
                      (triggered colfoldimg >>>
                         imgSelected colfoldimg colorfolderImg) +>
                      (triggered numfoldimg >>>
                         imgSelected numfoldimg numfolderImg) +>
                      (triggered quit >>> destroy tk) +>
                      (selectedItemInTreeList gui >>>=
                         selectedTl foldlab) +>
                      (doubleClickInNotepad gui >>>= doubleClickNp))
-}
{-
    spawnEvent (forever ((selectedItemInTreeList gui >>>=
                            selectedTl foldlab) +>
                         (doubleClickInNotepad gui >>>=
                            doubleClickNp)))
-}
    addExampleFolders newID gui
    (htk_destr, _) <- bindSimple main Destroy
    sync (htk_destr)
    finishHTk main

imgSelected :: Button () -> IO Image -> IO ()
imgSelected but img =
  do
    but' <- getRef lastactiveref
    if but' == but then done else
      do
        but' # relief Sunken
        but # relief Raised
        setRef lastactiveref but
        setRef imgref img

selectedTl :: Label String -> Maybe (Item Obj) -> IO ()
selectedTl foldlab mitem =
  case mitem of
    Nothing -> foldlab # text "no folder selected" >> done
    Just item -> let val = content item
                 in case val of
                      (nm, _, _) ->
                        do
                          foldlab # text ("'" ++ full nm ++ "'")
                          setRef foldref (Just item)
                      _ -> setRef foldref Nothing

doubleClickNp :: Item Obj -> IO ()
doubleClickNp item = done
{-
  let newitem = content item
  in case newitem of
       LeafItem ext ->
         do
           val <- get ext :: IO Dynamic
           case fromDynamic val :: Maybe (IO Image) of
             Just ioimg -> do
                             main <- createToplevel [text "Image"]
                             img <- ioimg
                             lab <- newLabel main [photo img] ::
                                      IO (Label Image)
                             pack lab []
                             quit <- newButton main [text "Close"]
                                       :: IO (Button String)
                             pack quit []
                             clickedquit <- clicked quit
                             spawnEvent (forever (clickedquit >>
                                                  always (destroy main)))
                             done
             _ -> done
           case fromDynamic val :: Maybe String of
             Just str -> do
                           main <- createToplevel [text "Text"]
                           lab <- newLabel main
                                    [text ("     " ++ str ++ "     "),
                                     height 5, relief Sunken,
                                     font (Helvetica, 12 :: Int)] ::
                                  IO (Label String)
                           pack lab []
                           quit <- newButton main [text "Close"]
                                     :: IO (Button String)
                           pack quit []
                           clickedquit <- clicked quit
                           spawnEvent (forever (clickedquit >>
                                                always (destroy main)))
                           done
             _ -> done
           case fromDynamic val :: Maybe Col of
             Just mycolor -> do
                               main <- createToplevel [text "Color"]
                               lab <- newLabel main
                                        [relief Sunken, size (20,8),
                                         font (Helvetica, 18 :: Int),
                                         bg (case mycolor of
                                               Red -> "red"
                                               Green -> "green"
                                               Blue -> "blue"
                                               Yellow -> "yellow"),
                                         text (case mycolor of
                                                  Red -> "Red"
                                                  Green -> "Green"
                                                  Blue -> "Blue"
                                                  Yellow -> "Yellow")]
                                        :: IO (Label String)
                               pack lab []
                               quit <- newButton main [text "Close"]
                                         :: IO (Button String)
                               pack quit []
                               clickedquit <- clicked quit
                               spawnEvent
                                 (forever (clickedquit >>
                                           always (destroy main)))
                               done

             _ -> done
           case fromDynamic val :: Maybe Int of
             Just n -> do
                         main <- createToplevel [text "Number"]
                         lab <- newLabel main
                                  [text (show n), relief Sunken,
                                   size (20,8),
                                   font (Helvetica, 18 :: Int)] ::
                                IO (Label String)
                         pack lab []
                         quit <- newButton main [text "Close"]
                                   :: IO (Button String)
                         pack quit []
                         clickedquit <- clicked quit
                         spawnEvent (forever (clickedquit >>
                                              always (destroy main)))
                         done
             _ -> done
       _ -> done
-}


exportState :: GenGUI Obj -> IO ()
exportState gui =
  do
    st <- exportGenGUIState gui
    putStrLn "state exported"
    gui_clone <- newGenGUI (Just st)
    putStrLn "state imported"


------------
-- images --
------------

yellowImg = newImage NONE [imgData GIF "R0lGODlhDAAMAIQAAOfkFuTgHOHdId3aJtrXLNbTMdPQNtDNPMzKQcnGRsXDS8LAUb+9Vru5W7i2
YbSzZrGva62scKqpdqeme6OigKCfhpyci5aWlpaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWliH+FUNy
ZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgAfACwAAAAADAAMAAAFPyAQCANRGAeSKMtYnum6MKSJ
qizTvLesO7ZYruF4wHAz4gPSGxYhEWHyGZEgf8vqxKeESiYUZ/ZLqUzH4IolBAA7"]

greenImg = newImage NONE [imgData GIF "R0lGODlhDAAMAIQAAAD/IQb6Jgz2KxLxMBntNR/pOiXkPyvgQzLcSDjXTT7TUkTOV0vKXFHGYFfB
ZV29amS4b2q0dHCweXarfX2ngoOjh4mejJaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWliH+FUNy
ZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgAfACwAAAAADAAMAAAFPyAQCANRGAeSKMtYnum6MKSJ
qizTvLesO7ZYruF4wHAz4gPSGxYhEWHyGZEgf8vqxKeESiYUZ/ZLqUzH4IolBAA7"]

blueImg = newImage NONE [imgData GIF "R0lGODlhDAAMAIQAAAA//wZC+gxG9hJJ8RlN7R9R6SVU5CtY4DJc3Dhf1z5j00RmzktqylFuxldx
wV11vWR5uGp8tHCAsHaDq32Hp4OLo4mOnpaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWliH+FUNy
ZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgAfACwAAAAADAAMAAAFPyAQCANRGAeSKMtYnum6MKSJ
qizTvLesO7ZYruF4wHAz4gPSGxYhEWHyGZEgf8vqxKeESiYUZ/ZLqUzH4IolBAA7"]

redImg = newImage NONE [imgData GIF "R0lGODlhDAAMAIQAAP8UAPoaBvYfDPElEu0qGekvH+Q1JeA6K9xAMtdFONNKPs5QRMpVS8ZaUcFg
V71lXbhrZLRwarB1cKt7dqeAfaOFg56LiZaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWlpaWliH+FUNy
ZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgAfACwAAAAADAAMAAAFPyAQCANRGAeSKMtYnum6MKSJ
qizTvLesO7ZYruF4wHAz4gPSGxYhEWHyGZEgf8vqxKeESiYUZ/ZLqUzH4IolBAA7"]

txtImg = newImage NONE [imgData GIF "R0lGODlhDAAMAKEAAAAAAP///9jY2NjY2CH5BAEKAAIALAAAAAAMAAwAAAIjlIEJduEflgAwxUXh
Q/Pa22kORyEemIlk5JWYOErKLE00UgAAOw=="]

numImg = newImage NONE [imgData GIF "R0lGODlhDAAMAKUAAP////v7+/j4+PX19fHx8e7u7uvr6+jo6OXl5eHh4d7e3tvb2/ba2vWgoPOe
nvGcnOTIyOWtrfw2Nvs1Nf8AAO21teuWlvRKSv03N/LW1vkzM/vf3/tRUfdNTfNmZumUlPynp/sZ
GfVLS/e/v/KBge59feKqqv0bG/S8vPOCguqysu5hYfjc3PptbflPT+rOzueSktjY2NjY2NjY2NjY
2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2CH5BAEKAD8ALAAAAAAMAAwAAAZlQEBA
MCAUDAdEQrEQChgNxyMJiTSHkomDUkFYLleMJHPUIChgwIbCMRo6HsqnCaIUjwdKyAqo35EUIkx9
FCNuJBQlg3VtRx2BCiZCFCcoBSkUKiEXK0IsLScuFC9eFDBOf0lLC0EAOw=="]

imgImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAHq86Oz8FP///wAAAHqk/yrMS1XRYP///yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAHACwAAAAADAAMAAADL3i6vPAwKhjimyBoIQ8YBCEI4nUMYCiAZlGgMNEWRl0P
ra3jiuvqhkHPR3Q1jooEADs="]

folderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAAP///////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAEACwAAAAADAAMAEADLUi6vCAihDjBIA9qaBWYAtBgkESFF6Cu7OWpI3myXlSW
3QPueTmZHcJgSCwmAAA7"]

imgfolderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgHqk/+z8FP///wAAAFXRYP///////yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAHACwAAAAADAAMAAADMHi63PBtgSACiFPo8eABRBAMw2gRXygOKvgVRByLLlDc
eFzjPLHzuFpn+EAdZEhZAgA7"]

txtfolderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAANjY2NjY2NjY2NjY2CH5BAEKAAEALAAAAAAMAAwAAAMo
GLrc8G0BQUGctD4b5viZAAxdGI7lIHyqSGKmm66sDJvopm9kwP6sBAA7"]

numfolderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAOMAAICAgP///wAAAP/j4/+Ojv9ycv+qqv/Hx/9VVdjY2NjY2NjY2NjY2NjY2NjY
2NjY2CH5BAEKAA8ALAAAAAAMAAwAAAQz8MlJK7h1gsBB3lx3eQ8YhoBQBgNRGEenbogRIOh827i8
Gi+CLZBanXKlkfKiegie0GgEADs="]

colorfolderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP////8AAD3yKQAAAP//AAAz/9jY2CH5BAEKAAcALAAAAAAMAAwAAAMy
eLrc8G2BQEGctD57sPjDEABE9wlBGBSkmYZFYbSTKhtjCcB3pVM9X2dDfJQOhKRymQAAOw=="]

img1 = newImage NONE [imgData GIF "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
OLCNIwICADs="]

img2 = newImage NONE [imgData GIF "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"]