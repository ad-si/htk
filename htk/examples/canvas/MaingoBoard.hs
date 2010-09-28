-- | Contributed by Isaac Jones (ijones\@syntaxpolice.org).  From the README
-- file:
--
-- This simple program is released by Isaac Jones under a
-- \"BSD-Style\" license.
--
-- You may do with it as you like.
-- ----------------------------------------------------------------------


module Main (main) where

import HTk.Toplevel.HTk

lightBrown :: (Int, Int, Int)
lightBrown = (224, 186, 145)

stoneSize :: Distance
stoneSize = 30

blackFill = filling "black"
whiteFill = filling "white"

-- Constants and functions for line grid
lineStart = 35
lineWidth = outlinewidth (mm 1)

--horizStartPoint :: Int -> (Distance, Distance)
horizStartPoint linesLeft            = (lineStart,     cm linesLeft)
horizEndPoint   totalLines linesLeft = (cm totalLines, cm linesLeft)
vertStartPoint  linesLeft            = (cm linesLeft,  lineStart)
vertEndPoint    totalLines linesLeft = (cm linesLeft,  cm totalLines)

canvasSize :: Double -> Config Canvas
canvasSize boardSize = size (cm (boardSize + 1), cm (boardSize + 1))

-- |Figure out whose turn it is.  This returns the value from this tk
-- variable and also toggles it so the players take turns.

getAndToggle v r1 r2 =
    do
    val1 <- readTkVariable v
    (if val1 == 1 then (do
                        invoke r2
                        return (blackFill))
                  else (do
                        invoke r1
                        return whiteFill))


-- |Create an exciting grid of a certain size.  Hope to scale the
-- board on resize sometime.
makeLines totalLines 0 windowSize cnv = return ()
makeLines totalLines linesLeft windowSize cnv =
    do
    -- Vertical lines:
    createLine cnv [coord [vertStartPoint linesLeft, vertEndPoint totalLines linesLeft],
                    capstyle CapRound, joinstyle JoinMiter,
                    lineWidth, filling "black"]

    -- Horizontal lines:
    createLine cnv [coord [horizStartPoint linesLeft, horizEndPoint totalLines linesLeft],
                    capstyle CapRound, joinstyle JoinMiter,
                    lineWidth, filling "black"]

    makeLines totalLines (linesLeft - 1) windowSize cnv
    return ()

-- |create the board, populating the elements with their events, draw
-- the grid, hand black a stone.
makeBoard main uBoardSize newClicked toggleFunc =
  do
    boardSize <- readTkVariable uBoardSize
    cnv <- newCanvas main [canvasSize boardSize, background lightBrown]
    (press, _) <- bind cnv [WishEvent [] (ButtonPress (Just 1))]
    makeLines boardSize boardSize (boardSize + 1) cnv
    spawnEvent (forever
                 (do (x, y) <- press >>>= \i-> return (x i, y i)
                     always (do
                             fill <- toggleFunc
                             spawn (drawStone (x,y) cnv fill))))
    pack cnv []
    spawnEvent (newClicked >>> (do
                                destroy cnv
                                makeBoard main uBoardSize newClicked toggleFunc
                                return()
                               ))

drawStone (x, y) cnv fill =
       do
       stone <- createOval cnv [fill, size (stoneSize, stoneSize),
                                position (x - (stoneSize `div` 2),
                                          y - (stoneSize `div` 2))]
       return()
------------------------------------------------------------
main :: IO ()
main =
  do
    main <- initHTk [text "Go"]
    -- create menus:
    menuBar  <- createMenu main False []
    main # menu menuBar

    -- file menu:
    fileMenu <- createPulldownMenu menuBar [text "File"]
    quitMenu <- createMenuCommand fileMenu [text "Quit"]

    -- game menu:
    gameMenu     <- createPulldownMenu menuBar [text "Game"]
    uBoardSize   <- createTkVariable (19::Double)
    sizeNine     <- createMenuRadioButton gameMenu [text "9x9", value (9::Double),
                                                    variable uBoardSize]
    sizeThirteen <- createMenuRadioButton gameMenu [text "13x13", value (13::Double),
                                                    variable uBoardSize]
    sizeNineTeen <- createMenuRadioButton gameMenu [text "19x19", value (19::Double),
                                                    variable uBoardSize]
    newGame <- createMenuCommand gameMenu [text "New Game"]
    pack menuBar []

    quitClicked <- clicked quitMenu
    newClicked  <- clicked newGame

    -- whose turn is it toggle buttons:
    box <- newVBox main []
    v <- createTkVariable (1::Int)
    r1 <- newRadioButton box [text "Black's Move", value (1::Int), variable v]
    r2 <- newRadioButton box [text "White's Move", value (2::Int), variable v]
    pack box []
    pack r1 []
    pack r2 []

    makeBoard main uBoardSize newClicked (getAndToggle v r1 r2)

    spawnEvent (quitClicked >>> do destroy main)

    finishHTk
