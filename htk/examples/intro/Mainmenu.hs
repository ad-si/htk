module Main (main) where
import HTk.Toplevel.HTk

main :: IO ()
main =
 do main <- initHTk [text "Menus!", size(300, 300)]

    menubar <- createMenu main False []
    main # menu menubar

    pd1 <- createMenuCascade menubar [text "First Menu"]
    m1 <- createMenu menubar False []
    pd1 # menu m1

    c1 <- createMenuCommand m1 [text "First Menu Point"]
    c2 <- createMenuCommand m1 [text "Second Menu Point"]
    createMenuSeparator m1 []
    s   <- createPulldownMenu m1 [text "Submenu"]
    c31 <- createMenuCommand s [text "First Subpoint"]
    c32 <- createMenuCommand s [text "Second Subpoint"]

    m2 <- createPulldownMenu menubar [text "Buttons"]
    v1 <- createTkVariable True
    c1 <- createMenuCheckButton m2 [text "I am cool", variable v1]

    createMenuSeparator m2 []

    v2 <- createTkVariable (0::Int)
    r1 <- createMenuRadioButton m2 [text "No milk or sugar",
                                    value (0::Int), variable v2]
    r2 <- createMenuRadioButton m2 [text "Milk, no sugar",
                                    value (1::Int), variable v2]
    r3 <- createMenuRadioButton m2 [text "Sugar and milk",
                                    value (2::Int), variable v2]

    cl <- clicked c1
    spawnEvent (forever (
         (cl >>> do val1 <- readTkVariable v1
                    val2 <- readTkVariable v2
                    putStrLn ("v1: "++  show val1++
                              ", v2: "++ show val2))))
    finishHTk

