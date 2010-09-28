
module Main (main) where

import HTk.Toplevel.HTk

main :: IO ()
main =
  do
    main <- initHTk [text "HTk CanvasExample"]
    c <- newCanvas main [size (cm 20, cm 15), background "white"]
    pack c []

    santa <- newImage  [filename "./images/santa.gif", imgGamma 0.5, imgPalette (4::Int, 4::Int, 4::Int)]
    frosty <- newImage [filename "./images/snowman.gif"]
    jingle<- newImage  [filename "./images/bells.gif", imgGamma 5.0, imgPalette (128::Int)]

    createImageItem c [position (cm 3, cm 5), photo santa]
    createImageItem c [position (cm 7, cm 5), photo frosty]
    createImageItem c [position (cm 10, cm 5), photo jingle]

    createTextItem c [position (cm 4, cm 2), text "Merry Xmas!",
                      font (Helvetica, Bold, 24::Int)]

    createLine c [coord [(cm 2, cm 8), (cm 3, cm 9),
                         (cm 3, cm 8), (cm 2, cm 9), (cm 4, cm 8.5)],
                  capstyle CapRound, joinstyle JoinMiter,
                  outlinewidth (mm 1), arrowstyle LastEnd, filling "red"]

    createArc c [position (cm 5, cm 8), size (cm 1.5, cm 1.5), extent 110,
                 filling "green", outlinewidth (mm 1), outline "black"]

    b <- newButton c [text "Click me!", relief Raised]
    eb <- createEmbeddedCanvasWin c b [position (cm 2, cm 12)]
    clickedb <- clicked b
    spawnEvent (forever (clickedb >> always (putStrLn "click" >> bell)))
    (press, _) <- bind c [WishEvent [] (ButtonPress (Just 1))]
    (move, _) <- bind c [WishEvent [Button1] Motion]
    (release, _) <- bindSimple c (ButtonRelease (Just 1))

    let moving :: Distance -> Distance -> CanvasTag -> Event ()
        moving x0 y0 ct =
             (do
                (x, y) <- move >>>= \i-> return (x i, y i)
                always (moveItem ct (x - x0) (y - y0))
                moving x y ct)
          +> (release >> notmoving)

        notmoving :: Event ()
        notmoving = do
                      (x, y) <- press >>>= \i-> return (x i, y i)
                      ct <- always (do ct<- createCanvasTag c []
                                       addCanvasTag (closest (x, y)) ct
                                       return ct)
                      moving x y ct

    spawnEvent (forever notmoving)

    finishHTk
