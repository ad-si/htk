\begin{code}
module Main where


import Random -- (randomRIO)
import Array
import List (union, nub)
import System
import Monad (foldM)
import IOExts (unsafePerformIO)

import HTk hiding (State)
import DialogWin (createAlertWin,createMessageWin)

-- The state of a single field.
data State = Cleared Int 
           | Unexplored { flagged :: Bool,
	                  mine    :: Bool 
                        }

-- A field is still untouched if it is neither cleared nor flagged
untouched :: State-> Bool
untouched (Cleared _) = False
untouched (Unexplored{flagged = f}) = not f

-- Counts the number of mines on a field
mines :: State-> Int
mines (Unexplored{mine= True}) = 1
mines _                        = 0

-- Our playing field: an array of states, and the button handlers for 
-- them. (We keep them separate, since the state wil change,
-- and the button handlers don't.)
type Mines   = Array (Int, Int) State
type Buttons = Array (Int, Int) Button

-- get list of all adjacents to a given position
adjacents :: Mines-> (Int, Int)-> [(Int, Int)]
adjacents p (x, y) = 
  filter (inRange (bounds p))
         [(x-1, y-1), (x, y-1), (x+1, y-1),
          (x-1, y),             (x+1, y),
	  (x-1, y+1), (x, y+1), (x+1, y+1)]

-- Get a non-repeating infite list of valid positions.
rndPos :: Mines-> IO [(Int, Int)]
rndPos p = 
   do s1<- newStdGen
      s2<- newStdGen
      return (nub (zip (randomRs (lox, hix) s1) 
                       (randomRs (loy, hiy) s2))) where
        ((lox, loy), (hix, hiy))= bounds p
 
-- Create all mines
createMines :: (Int, Int)-> IO Mines
createMines (w, h) = 
  do -- We first get the positions for all the mines, and then put
     -- them on an empty pitch.
     minePos <- rndPos mt
     return (mt // zip (take numMines minePos) (repeat mine)) where
         mt= listArray ((1, 1), (w, h)) (repeat nomine)
         numMines = (w*h) `div` 5 
         mine   = Unexplored{mine= True, flagged= False}
         nomine = Unexplored{mine= False, flagged= False}

-- Peek at a list of fields, and count the number of
-- adjacent mines. If there are none, we recursively peek at all the
-- adjacent fields, which are
-- a. not already cleared, and
-- b. not on our list of fields to peek at
-- Precondition: all fields in the list are untouched.
peek :: Buttons-> Mines-> [(Int, Int)]-> IO Mines
peek b m [] = return m
peek b m (xy:rest) =
   let adjMines :: Int
       adjMines = sum (map (mines. (m !)) (adjacents m xy))
       nu       = m // [(xy, Cleared adjMines)]
   in do (b!xy)#(text (show adjMines))
         if adjMines == 0 then 
            peek b nu (rest `union` (filter (untouched. (m !))
                                            (adjacents m xy)))
            else peek b nu rest
  

-- open up a field (mouse left-click)
-- returns Nothing, if we click on a hidden mine, the input if we 
-- click on a flagged field (without a mine), and peeks at the field
-- otherwise
open :: Buttons-> Mines-> (Int, Int)-> IO (Maybe Mines)
open b m xy = 
  case m!xy of 
    Unexplored {mine= True}    -> return Nothing
    Unexplored {flagged= True} -> return (Just m)
    Cleared _                  -> return (Just m)
    _ -> peek b m [xy] >>= return. Just

-- drop or retrieve a flag (mouse right-click) 
flag :: Buttons-> Mines-> (Int, Int)-> IO Mines
flag b m xy = 
  case m!xy of
    Cleared _ -> return m
    s@(Unexplored{flagged= f})-> 
        do b!xy # (if f then text "X" else text " ")
           return (m // [(xy, s{flagged= not f})])
                                 
-- create all buttons, and set up the handlers for them.
-- Returns a list of pairs of buttons, and their position.
buttons :: Container par=> par-> Button-> Event() -> (Int, Int)
                           -> IO [((Int, Int), Button)]
buttons par sb startEv (size@(xmax, ymax)) =
  do buttons <- mapM (\xy-> do b<- newButton par [text "?"]
                               return (xy, b)) [(x, y) | x <- [1.. xmax],
                                                         y <- [1.. ymax]]
     let bArr = array ((1,1), size) buttons
         getButtonClick b n xy = 
            do (click, _) <- bindSimple b (ButtonRelease (Just n))
               return (click >> return xy)
     leCl  <- mapM (\(xy, b)-> getButtonClick b 1 xy) buttons
     riCl  <- mapM (\(xy, b)-> getButtonClick b 3 xy) buttons
     press <- mapM (\(_, b)-> do (cl, _)<- bindSimple b (ButtonPress Nothing)
                                 return cl) buttons
     let -- game ended:
         gameLost :: IO ()
         gameLost = do sb # photo smSadImg
		       createAlertWin "*** BOOM!***\nYou lost." []
         gameWon :: IO ()
	 gameWon = do  sb # photo smWinImg
		       createMessageWin "You have won!" []
         playOn :: Mines-> Event ()
         playOn m = do always (sb # photo smCoolImg)
                       play m       
         -- the button handlers: 
         play :: Mines-> Event ()
         play m = do r <- choose leCl >>>= open bArr m
                     case r of Nothing -> always gameLost >> gameOver
                               Just nu -> -- for simplicity, we have won
			                  -- if no untouched fields are left
			                  if all (not.untouched)
					                   (elems nu) then 
			                    always gameWon >> gameOver
                                          else playOn nu
                  +>
                  do r<- choose riCl >>>= flag bArr m
                     playOn r
                  +>
                  do choose press 
		     always (sb # photo smWorriedImg >> done) 
                     play m
                  +>
                  start 
         start :: Event ()
         start = startEv >>> do m <- createMines (snd (bounds bArr))
                                mapM_ (text " ") (elems bArr)
                                sync (play m)
         gameOver :: Event ()
         gameOver = start 
                    +> 
                    (choose (leCl++ riCl) >> gameOver) 

     spawnEvent start
     return buttons

main :: IO ()
main = 
 do main <- initHTk [text "hsMines"]
    -- Main window menu 
    menubar <- createMenu main False []
    main # menu menubar

    fm <- createPulldownMenu menubar [text "File"]
    b2 <- createMenuCommand fm [text "Restart"]
    restartClick <- clicked b2
    b3 <- createMenuCommand fm [text "Quit"]
    quitClick <- clicked b3

    em <- createPulldownMenu menubar [text "Preferences"]
    _ <- createMenuCommand em [text "Cut"]
    -- clickedb2 <- clicked b2
    _ <- createMenuCommand em [text "Copy"]
    -- clickedb3 <- clicked b3
    _ <- createMenuCommand em [text "Paste"]
    -- clickedb4 <- clicked b4

    -- create the smiley button on top the mines
    sm <- newButton main [photo smSmileImg]
    restart2Click <- clicked sm

    restartCh <- newChannel

    let size= (20, 20)

    bfr <- newFrame main [width (cm 10)]
    allbuttons <- buttons bfr sm (receive restartCh) size 

    pack sm [Side AtTop, PadY 20, PadX 20, Anchor North]
    pack bfr [Side AtTop, PadX 15] 
    mapM_ (\(xy, b)-> grid b [GridPos xy]) allbuttons
    
    -- start the menu handler
    spawnEvent (forever (restartClick >>> sendIO restartCh ()
                      +> restart2Click >>> sendIO restartCh ()
                      +> quitClick >>> destroy main))

    -- start the game
    sendIO restartCh ()
   
    -- wait for game to stop, then clear up the mess
    finishHTk
   
\end{code}

These are the bitmaps for the graphics, taken from gnomines. 

\begin{code}

flagImg :: Image
flagImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhDAALAPEAAAAAAP4CBLLA3AAAACwAAAAADAALAAACGpQfcCvhypyERlqKLstpL4BtoHeMHohi6QoUADs=" ])

{-# NOINLINE flagImg #-}

smCoolImg :: Image
smCoolImg = unsafePerformIO (newImage NONE [imgData GIF "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"])

{-# NOINLINE smCoolImg #-}


smSadImg :: Image
smSadImg = unsafePerformIO (newImage NONE [imgData GIF "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"])

{-# NOINLINE smSadImg #-}


smSmileImg :: Image
smSmileImg = unsafePerformIO (newImage NONE [imgData GIF "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" ])

{-# NOINLINE smSmileImg #-}


smWorriedImg :: Image
smWorriedImg = unsafePerformIO (newImage NONE [imgData GIF "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" ])

{-# NOINLINE smWorriedImg #-}

smWinImg :: Image
smWinImg = unsafePerformIO (newImage NONE [imgData GIF "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"])

{-# NOINLINE smWinImg #-}





\end{code}
