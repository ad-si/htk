
module Main (main) where

import HTk.Toplevel.HTk

main :: IO ()
main =
  do
    main <- initHTk [text "Menus!", size(300, 300)]

    menubar <- createMenu main False []
    main # menu menubar

    pulldown1 <- createMenuCascade menubar [text "First Menu"]
    m <- createMenu main True []
    pulldown1 # menu m

    v1 <- createTkVariable "1"
    c1 <- createMenuCheckButton m [text "MenuCheckButton", variable v1]
    clickedc1 <- clicked c1

    b1 <- createMenuCommand m [text "b1"]
    clickedb1 <- clicked b1

    createMenuSeparator m []

    c <- createMenuCascade m [text "Cascade"]
    cm <- createMenu main True []
    c # menu cm

    v2 <- createTkVariable "r2"
    r1 <- createMenuRadioButton cm [text "Select 1", value "r1",
                                    variable v2]
    r2 <- createMenuRadioButton cm [text "Select 2", value "r2",
                                    variable v2]
    r3 <- createMenuRadioButton cm [text "Select 3", value "r3",
                                    variable v2]

    clickedr1 <- clicked r1
    clickedr2 <- clicked r2
    clickedr3 <- clicked r3

    v3 <- createTkVariable "1"
    c2 <- createMenuCheckButton m [text "Check", variable v3]
    clickedc2 <- clicked c2

    pulldown2 <- createMenuCascade menubar [text "Second Menu"]
    m2 <- createMenu main False []
    pulldown2 # menu m2

    b2 <- createMenuCommand m2 [text "Some Text Item"]
    clickedb2 <- clicked b2

    b3 <- createMenuCommand m2 [text "Some Other Item"]
    clickedb3 <- clicked b3

    v4 <- createTkVariable (0 :: Int)
    ra <- createMenuRadioButton m2 [text "Select A", value (1 :: Int),
                                    variable v4]
    clickedra <- clicked ra
    rb <- createMenuRadioButton m2 [text "Select B", value (2 :: Int),
                                    variable v4]
    clickedrb <- clicked rb

    v5 <- createTkVariable (0.2 :: Double)

    b <- newButton main [text "show selection above"]
    pack b [Side AtBottom, PadY 10]
    clickedb <- clicked b

    rw1 <- newRadioButton main [text "RadioButton 1", variable v5,
                                value (0.1 :: Double)]
    rw2 <- newRadioButton main [text "RadioButton 2", variable v5,
                                value (0.2 :: Double)]
    rw3 <- newRadioButton main [text "RadioButton 3", variable v5,
                                value (0.3 :: Double)]
    pack rw3 [Side AtBottom]
    pack rw2 [Side AtBottom]
    pack rw1 [Side AtBottom]

    v6 <- createTkVariable True
    cw <- newCheckButton main [text "CheckButton", variable v6]
    pack cw [Side AtBottom, PadY 10]
    clickedcw <- clicked cw

    mbutton <- newMenuButton main [text "MenuButton"]
    pack mbutton [Side AtBottom]

    buttonmenu <- createMenu mbutton True []
    cmd1 <- createMenuCommand buttonmenu [text "command1"]
    cmd2 <- createMenuCommand buttonmenu [text "command2"]
    cmd3 <- createMenuCommand buttonmenu [text "command3"]
    mbutton # menu buttonmenu

    clickedcmd1 <- clicked cmd1
    clickedcmd2 <- clicked cmd2
    clickedcmd3 <- clicked cmd3

    men <- createMenu main False []

    cmda <- createMenuCommand men [text "command a"]
    cmdb <- createMenuCommand men [text "command b"]
    cmdc <- createMenuCommand men [text "command c"]

    casc <- createMenuCascade men [text "Cascade Menu"]
    cascmenu <- createMenu casc False []
    var1 <- createTkVariable "r2"
    rad1 <- createMenuRadioButton cascmenu [text "Select 1", value "r1",
                                            variable var1]
    rad2 <- createMenuRadioButton cascmenu [text "Select 2", value "r2",
                                            variable var1]
    rad3 <- createMenuRadioButton cascmenu [text "Select 3", value "r3",
                                            variable var1]
    casc # menu cascmenu

    ca <- clicked cmda
    cb <- clicked cmdb
    cc <- clicked cmdc

    let menusel = ca >>> click "A" +> cb >>> click "B" +> cc >>> click "C"

    (press, _) <- bind main [WishEvent [] (ButtonPress (Just 3))]


     -- A pop-up menu can finish without it doing anything, hence we
     -- cannot just sync on menusel above after popping up.
     -- In particular, the following does not occur when the pop-up
     -- menu disappears:
     -- (eop, _) <- bindSimple men Destroy
     -- Solution: have one thread handle the pop-up menu exclusively.


    spawnEvent (forever ((clickedc1 >> always (selected "c1" v1)) +>
                         (clickedb1 >> always (click "b1")) +>
                         (clickedr1 >> always (selected "r1"  v2)) +>
                         (clickedr2 >> always (selected "r2"  v2)) +>
                         (clickedr3 >> always (selected "r3"  v2)) +>
                         (clickedc2 >> always (selected "c2" v3)) +>
                         (clickedb2 >> always (click "b2")) +>
                         (clickedb3 >> always (click "b3")) +>
                         (clickedra >> always (selected "ra" v4)) +>
                         (clickedrb >> always (selected "rb" v4)) +>
                         (clickedb >>
                          always (do
                                    i <- readTkVariable v5
                                    putStrLn ("value is " ++ show i))) +>
                         (clickedcw >> always (selected "cw" v6)) +>
                         (clickedcmd1 >> always (click "cmd1")) +>
                         (clickedcmd2 >> always (click "cmd2")) +>
                         (clickedcmd3 >> always (click "cmd3")) +>
                         menusel +>
                         (press >>>=
                            \eventInfo ->
                              do popup men (xRoot eventInfo,
                                            yRoot eventInfo)
                                           (Nothing :: Maybe HTk)
                                 putStrLn "We've popped up")))
    finishHTk

  where selected :: GUIValue a => String -> TkVariable a -> IO ()
        selected nm var =
          do
            val <- readTkVariable var
            putStrLn (nm ++ " " ++ show val)

        click :: String -> IO ()
        click nm = putStrLn (nm ++ " clicked")
