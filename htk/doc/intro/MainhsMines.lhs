\begin{code}
module Main where


import Random -- (randomRIO)
import Array
import List (union, nub)
import System
import Monad (foldM)

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
        do b!xy # (text (if f then " " else "F"))
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
            do (click, _) <- bindSimple b (ButtonPress (Just n))
               return (click >> return xy)
     leCl <- mapM (\(xy, b)-> getButtonClick b 1 xy) buttons
     riCl <- mapM (\(xy, b)-> getButtonClick b 3 xy) buttons
     let -- game ended:
         gameLost :: IO ()
         gameLost = do createAlertWin "*** BOOM!***\nYou lost." []
                       sb # (text "X-(")
                       done
         gameWon :: IO ()
	 gameWon = createMessageWin "You have won!" []
         -- the button handlers: 
         play :: Mines-> Event ()
         play m = do r <- choose leCl >>>= open bArr m
                     case r of Nothing -> always gameLost >> gameOver
                               Just nu -> -- for simplicity, we have won
			                  -- if no untouched fields are left
			                  if all (not.untouched)
					                   (elems nu) then 
			                    always gameWon >> gameOver
                                          else play nu
                  +>
                  do r<- choose riCl >>>= flag bArr m
                     play r
                  +>
                  start 
         start :: Event ()
         start = startEv >>> do m <- createMines (snd (bounds bArr))
                                mapM_ (text " ") (elems bArr)
				sb # (text ":-)")
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
    sm <- newButton main [text ":-)"]
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
