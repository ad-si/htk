\section{The HsMines Game}
As an example for more complex GUI programming, we will now develop a
GUI for a Minesweeper like game called HsMines. Just in case you do
not know Minesweeper or some of its many clones I'll give a short
overview.

\subsection{What's that game?}
Playing Minesweeper you have a grid of about 15x15 similar fields.
Hidden inside these fields could be a mine or just an empty field --
you wont know until you click at the field and are eventually shred to
pieces by some nasty mine. Goal of the game is to find all the mines
and mark them with tiny flags. An nearly impossible task. To make the
game any fun you are told the exact amount of mines around the field
you just opened. If this number is 0, it's safe to explore all the
adjacent fields. And because this is a stupid task the machine does it
for you. If you manage to explore all empty fields you win, if you
find one of the mines, you lose, best time gets the highscore.  Simple
as that.

For this is not a course in haskell programming I will not spent many
words on the games code itself and will come straight to the very
heart of this section, the HsMines GUI.

% Here is all of the code I did not talk about but that is
% needed to make the programm compileable

\begin{comment}
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

-- The field sizes. We need to type them explicitly, so we might as
-- well write them all down here
tinySize, weeSize, normalSize, bigSize, hugeSize :: (Int, Int)
tinySize = (6, 6)
weeSize  = (10, 10)
normalSize = (15, 15)
bigSize    = (20, 20)
hugeSize   = (25, 25)


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
createMines :: (Int, Int) -> Int 
                          -> IO Mines
createMines (w, h) d = 
  do -- We first get the positions for all the mines, and then put
     -- them on an empty pitch.
     minePos <- rndPos mt
     return (mt // zip (take numMines minePos) (repeat mine)) where
         mt= listArray ((1, 1), (w, h)) (repeat nomine)
         numMines = (w*h) `div` d
         mine   = Unexplored{mine= True, flagged= False}
         nomine = Unexplored{mine= False, flagged= False}
  
getImg :: Int -> Image
getImg x 
  | x == 1 = oneImg
  | x == 2 = twoImg
  | x == 3 = threeImg
  | x == 4 = fourImg
  | x == 5 = fiveImg
  | x == 6 = sixImg
  | x == 7 = sevenImg
  | x == 8 = eightImg
  | otherwise = zeroImg

zeroImg :: Image
zeroImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAP///2AAABAAAAwAAACwYQA5fgAGBABAQHwE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEAEEAACsAAEAAABT4NLcvZRAUEEAICDYnBOyAAAcEAEBAAAOc+AA4LwAUFAAICCAEJzYAgBQABAgAQFz4BAIvAAAUAAAIAPcE+DkALwwAFAgACKT4J/QvgP8UBL8IQBQnCLeANxAEFEBACNTowPM28f8U/78Iv2fEBPfyAAf/AEC/AJgEYTUAfiBhADZ+ABQEAAhAAFzcAALAAACyAAAAADQEAPQAAP8AAL8AACQU4gC35QAQGgjw/1jyAAT/kQRnAPT3JP8Hkf0EAAYAAJj4aDUv8xQU/wgIvyAnATaAAFwcMAI3ZQAUEND0FCvytwb/EEC/QLAEAGwAABQAAMBhOE1+8isE/0BAvwAATQAAqAAAGQAAQBQEVLcAQRAAK0AAQPgULC+38hQQ/whAv1gEVPQAQf8AK78AQCH4AEAvAAgUAAgIAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4wPQvTf8UK/iwAMNsABQUAABk1AA48gAU/wAIvw6+A4iCAAQEAEy+FAKCtyAEEANAQMgUwPS3Tf8QK79AQMwymPSNNf8EFL9ACPgUTMO38gAplgAA7AAABxf4A4IvAAcUAPgU1AEEAgD4AAAvAADcAAE4AAC3twAQENQEBNAU+PS3L/8QFMwEBIj4+PQvL/8UFL8ICFGMJ4LzgAf/BAi/QPjqcMO4OBQfFAhACBBUcABB8gAr/wBAvxB7BABgAAAQAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIHQABCBxIsKDBgwgTKlzIsKHDhxAjSpxIsaLFgQEBADs="])

{-# NOINLINE zeroImg #-}

oneImg :: Image
oneImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAEgO9HFE9oVe+Jp6+cKv+9fK/evl/v///3wE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAICwAaxsKBUUBggIQAAoXAA4OA6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAIAU5Ky3NhUQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAIAUCKy3ABUQAAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QIDqNKy4OBUfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIOwAPCBxIsKDBgwgTJiQwIIBCgQIASHx4oIABAgAoCsSo8QBHjR8phkxYgCEAAgQURpQosUDHlzBjGgwIADs=" ])

{-# NOINLINE oneImg #-}

twoImg :: Image
twoImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAD2eJVOpPn6+bpTJhqnUnr7fttTqz+r05////2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAJCwAcZsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAJAU5Ma3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAJAUCMa3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QJDqNMa4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIRwARCBxIsKDBgwgTIjwwAACAAQcUNiRAIMAAhQEICCQAQCFBAQE8ChQAoIDHAyRNKjwQoKRHAy0JFOCYsIDDmx1F6tzJc2dAADs=" ])

{-# NOINLINE twoImg #-}

threeImg :: Image
threeImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAPQdFvU3MPZQSvhoZPmCfvqbmPu0sf3NzP7m5f///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvADCwAbNsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EADAU5LO3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUADAUCLO3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QDDqNLO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAITAATCBxIsKDBgwgTIkRAAAAAAQgUDghQwEAAAQoRRExgAIBCgQcsDviYIIDDAyQFDgCwkSQAAwkJoESwsqVBkw5ffjTA02bKn0ARBgQAOw==" ])

{-# NOINLINE threeImg #-}

fourImg :: Image
fourImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAACk0k0FLn1lhq3B4t4mPw7i729DS5+jp8////2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvACiwAa5sKBUUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EACgU5K63NhUQFAhACAApvgAAggAABAAAQBf4A4IvAAcUACgUCK63ABUQAAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QCjqNK64OBUfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIQQARCBxIsKDBgwgTKkRAYICBhQgKAABQAGIAARQXEgBwIGPCjgQQeESIsYDEkAknqgQA8cBJiBsBBIBIs6ZNmwEBADs=" ])

{-# NOINLINE fourImg #-}

fiveImg :: Image
fiveImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAJYTE6IuLq1ISLliYsV9fdCWltywsOjLy/Pl5f///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAKCwAd9sKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAKAU5N+3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAKAUCN+3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QKDqNN+4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAISwATCBxIsKDBgwgTIjwwoOGAAgoNAHBoICIAhQQlBgAgAIHCAwAIGBAQAONABAAqmkyQMqEAiAkGAPCIUACAmy0/GjBAc6XPnwoDAgA7" ])

{-# NOINLINE fiveImg #-}

sixImg :: Image
sixImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAACdoIEB5OW+aaoishLfNtdDezufu5////3wE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAFCwAchsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAFAU5Mi3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAFAUCMi3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QFDqNMi4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAISwAPCBxIsKDBgwgTJjQgAAAAAgoLAAhAQIABhQECKBxIAEABAgU2DpjoUIDCkSY7hkTY8eKBhwsBpATgkqVDmRsPGACZs6fPnwcCAgA7" ])

{-# NOINLINE sixImg #-}

sevenImg :: Image
sevenImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAALwxrstfwNJ2ydqN0uGk2+m65PDS7fjo9v///2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAOCwAeNsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAOAU5OO3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAOAUCOO3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QODqNOO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIOwARCBxIsKDBgwgTIhTAsKFCAAEYBgCg0MABgQICKCQIgMBGgQUAXPyY8aPAjiZDjtw4QKPJlzBjfgwIADs=" ])

{-# NOINLINE sevenImg #-}

eightImg :: Image
eightImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAABi6rzLCuEzJwWXRyn/Z05ng3LLo5Mzw7eb39v///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhYTZ+fhQEBAhAQFzcAgLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEB1wcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAAhAAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAFCwAchsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAFAU+Mi3LxQQFAhACAApJwAAgAAABAAAQBf4A4IvAAcUAFAUCMi3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QFDqNMi4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIUAATCBxIsKDBgwgTJiwQAIAABAoLAChgIIAAhQMGCDQAIGIAiAMuKhQAoOQBhRwpCgiAUaNAAAYSEviYgACAkwgRNCxZQOFGAxB9Ch1KdGBAADs=" ])

{-# NOINLINE eightImg #-}


-- for testing purpose only

starImg :: Image
starImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhEAAQAPcAAAAAAB0dHTk5OVVVVXJyco6OjqqqqsfHx+Pj4////xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQE+UEAySsAFEAACBT4CbcvyhAUFEAICDYnBOyAAAcEAEBAAAOc+AA4LwAUFAAICCAEJzYAgBQABAgAQFz4BAIvAAAUAAAIAPcE+DkALwwAFAgACKT4J/QvgP8UBL8IQBQnCLeANxAEFEBACNTowPM28f8U/78Iv2fEBPfyAAf/AEC/AJgEYTUAfiBhADZ+ABQEAAhAAFzcAALAAACyADQEAPQAAP8AAL8AACQUVAC3QQAQKwBAQAjwBFjyAAT/AARnqPT3Yv8HB79ACP0EKCsA8gYA/0AAv5j4vjUvYhQUBwgICCAn2TaAyRQEFAhACFwcCQI3ytD0FCvytwb/EEC/QLAEAGwAABQAAMBhOE1+8isE/0BAvwAA0wAAKAAABgAAQBQEBLcAABAAAEAAAPgUOC+38hQQ/whAv1gEACH4AEAvAAgUAAgIAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4FPQvt/8UEPiwAcNsKBQUBggIQABk+A6+AIiCAAQEAEy+HAKCg+AEBAJAQMgUA/S3AP8QAMwyAPSNAP8EAPgU0MO3NhQQFAApvgAAggAABBf4A4IvAAcUAPgUCMO3ABQQAAEEAAD4AAAvAADcAAE4AAC3AAAQANQEBNAU+PS3L/8QFMwEBIj4+PQvL/8UFL8ICFGMJ4LzgAf/BAi/QPjqcMO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAITAATCBxIsKDBgwgTKlxY0MAAhgIdLkQQ8WECBBQLHggggAABAQUEACiA8ECBAAAIGFi4cQDJhQEMIAhwQCEBiwYCKKw5kCfEn0ATBgQAOw==" ])

{-# NOINLINE starImg #-}

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
smSadImg = unsafePerformIO (newImage NONE [imgData GIF "R0lGODdhIAAgAPcAALLA3NrWzOLanOjadOjWWOzWPOzSNubOPOXKN97KVNbKbNjOlNXVzN7asu7edPTeVPjaOPXWN+LGNOLGVOreh/reN/DaOu3SKOrONNzCNty+MdLGfNrazPbaJ/DaLNK6VPriI/neJfTWJ+7KMePCLdK2RM6yPM7KsfTaVOvWLOXKJ9y6LtLOsrmiGerLJ9KyLs6uNMauVOrafDo2DHZqEZyKFwICBGBWEBYWBMmyHMGqH2VaEcquH8umLrqylA0MBFNKDBUSBKaWFMq2HAYGBMqqLMSeKkxCDMKuFLqmHO/OK9iyKsKiRLqqbPXSOWZeFCoiBJ6OFAoGBBYOBN7CKNOuJsqiLL2aJrGOJzYyBMy6JPrqTPTiRHpuFEI6DObOHCYeB9GyIcqqTL2eKLiSJOrWdPrmXOrcXNzKTCAWBJJ+FOPCIti6H8umIq6ebO7SXPzuhM7CZMauIOW+L968IrmWIaqSTO7LP/zysP72tPTqoNrOdL6uPNi+HOK+I9i2KcamLMGWJK6KHKaGNP32yeLWfMCyQIZ2EF5SDG5eDLCYFM2qIqqGJKV+IvLSJt7OZPLqsP76zO7ifMa0NH5yDE5GDNC2HJx2H+rGROrFMO7mtJKCGG5iDJp6NN7ORLiwbFZOFKOSHKKOFOK6LNKqKZx6JJqCRN7KdOrGJKaWNL6mFKaCHp6OYN62MJ6GF5ZuHK6mjDYeFD0mFDoqDEYqFDQiDD4yDMqiJLaOJJBqHs66dKJKRLRKTKNFRE4iJHoyNH4+LJp+FK6GJJFuGpqKZNbStKxKS8ZWVMZSVMxVVIY9PMBOTGAsLJlyHY5mHMK+tLlOTNJWVNZaXHY2NJxCRH86PFxMDGsyNNxeXJJCRJI+PJJ2PII2NBoWBHpbFMaaJGxaFCAcBLq6pMJuPMBSVMTCtMKwdHJCLLqiZLZWRG46LMbGtLqSfK5mVIJSLJJuPJ+SZ7aujAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAIAAgAAAI/gABCBxIMICAAQQKGDiAIIGCBQwISpw4sIGDBxAiZDSwEIEECRMUBKBIksKDCigtWNh4AYNHCRk0bCBJkMNJlDg7eNhpoCWGjxo+fIhIssHNCiAqhFAKoYNOERcujJBAQkMJEycoNkABAedSpSHChuggIkVLFRI0rDBhgsVErjiVggARgq7YFi2gYnCRYcULGDEkykBRAULXEDNo1K1hI2yHGzhyXNCxg0dfGD18DOSQsWuHpTl+1AASREjdEEOC/LhBJEgRtSaKGMkKQIbGpmBBHCGCA8lcsUls2DiiRAWJFUt6MGkCIICT200/P7EBxYaQuXSjSBE+hYrxFVWs/lzBwkBAhPMiPofNMgQEDRtatnCx0MXLlxY/wLigoiGMmDFk+FCGE89Fpx52WlhgxhloZKBCCl/QkIYaa7DxQhtjYOHGG8+JEEF6YoUFwhZwwEFBHDkgwUYYbMjBwxx0vGBFHVjYcYcSToiQnlMi1gUHHnnosQcfUdQQhQ59rOHHH2EAEoggg9wxAo467jhWCgToQQgheBRiyCGIJKIIG1TQ8cciRtTBSCNSjuCEIzqqF8IXj0ASCSF5SDIJJZVUooYlZS5RxBVkDHIJJpmMMCWccYaQggIUaAKJHmdssgMniNTAxpJVAHKFII10MkEmiS5apU6exPHJJ4ZEAQon/qGIskgfo1hCSpqMlGLKKXPMgYoLSnwRlQgepJACGp+kEooah6jSBx1K0lEFhliscgkrC7QyyhxruADsF458ocYOiNwASp9AuNLCGnTQYQmaZOT6CiwBLNFKK374gcqvithARCyyzAIGFLTQUostriiyxCK3BIJLI5fkEpEuSywxyih+rMFDEDbQsgsvvfjySy+9ACOLDcG0gSEZwlwyDDECFUNKFVX8IW11UhhzDDLJKGMMMssYw8zAwdQRbyPNOPPMQLq0MfMSrtgQhCzQRCMNL9Pwkgwv1FTzgw3WrCKItbmwIpEYt9xCii02THFNMthIY0w2PPOizTRSpDHFkDAQ57LNRCwYYcQtzRAxBTfRxM2LMckkA83d3fjrTTO5KE3RCUZ8YwQ4NkRjdTRAA92LMszYEM7kw+QiDk0sMBEIGeNEk4znjZMDTdfMeHMJ5duUQ9NA5uAyDjnIEL8MNFxXc805rwzjjNm/E3QCOukgv3gv2iivjvPErBM9RcWw0w412nAzjTvvwBMPUSQFBAA7"])

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
\end{comment}

\subsection{In the beginning\dots}
\dots, as we have read, is the window. And for we are running compiled
code here, it's created in the \texttt{main} function.

\begin{code}
main = 
 do htk<- initHTk [withdrawMainWin]
    run htk normalSize 

run :: HTk-> (Int, Int)-> IO ()
run htk currentSize = 
 do main <- createToplevel [text "hsMines"]
\end{code}
\begin{verbatim}
    finishHTk
\end{verbatim}

Having only this, the GUI would look rather boring, but it's not bad
for just a few lines of code, isn't it? It looks a bit bent in a loop
but we will see later that this is needed to make the field
resizeable.

The last line is necessary to clean up all we've kicked loose so far.

\begin{figure}[h]
\begin{center}
\includegraphics[scale=0.6]{img/Screenshot01}
\caption{The hsMines window}
\end{center}
\end{figure}

\subsection{Menus}

What would a GUI be without them? Somewhat empty as it seems. So let's
create some menus.

\begin{code}
    menubar <- createMenu main False []
    main # menu menubar

    fm <- createPulldownMenu menubar [text "File"]

    restb <- createMenuCommand fm [text "Restart"]
    quitb <- createMenuCommand fm [text "Quit"]

    pm <- createPulldownMenu menubar [text "Preferences"]
   
    pmc1 <- createMenuCascade pm [text "Size"]
    pmc1m <- createMenu main False []
    pmc1 # menu pmc1m
        
    pmc2 <- createMenuCascade pm [text "Difficulty"]
    pmc2m <- createMenu main False []
    pmc2 # menu pmc2m
\end{code}

To this point we create a number of menus and pulldowns. Let's go
through this step by step.  In line 1 we create an \texttt{menu}
inside \emph{main} (our window) that was created in \texttt{run}. In
line 2 we tell \HTk to asign this new \texttt{menu} we call
\emph{menubar} to be the \texttt{menu} of \emph{main}.

In line 3 (we will not count the empty lines), we create our first
\texttt{pulldown menu} -- that is what actually is normaly called a
menu. This \texttt{pulldown menu} is created inside \emph{menubar}, is
called \emph{fm} and has the charming \texttt{text} "`File"'. For not
leaving this menu (or \texttt{pulldown menu}) empty, we create menu
entrys so called \texttt{menu commands} inside \emph{fm}. So by now we
have \emph{restb} and \emph{quitb} inside \emph{fm} inside
\emph{menubar} inside \emph{main}.

This is quit nice but we will try to put some more functionality into
our menu besides restarting and quiting the game. And as we read
above, the game grid will be resizeable so we will need a
"`Preferences"' menu to have these commands in.

We create a second \texttt{pulldown menu} in \emph{menubar} called
\emph{pm}. In this menu we nest to submenus called \texttt{menu
  cascades}. Each of the cascades has a name, \emph{pmc1} and
\emph{pmc2}, and a \texttt{String} set to it's \texttt{text}. The next
step is a bit tricky. One would expect now to fill the cascades
directly with some commands. But the \texttt{menu cascades} are only
\texttt{Containers} fit to hold a \texttt{menu}. So we have to create
another two menus inside \emph{main} and asign them to the two
cascades.  These two menus are called \emph{pmc1m} and \emph{pmc2m}
which should be an abreviation "`Prefernce Menu Cascade 1's Menu"'
respectivly 2.

By now we have a \texttt{menu} which holds two \texttt{pulldown
  menus}. The first contains two \texttt{commands}, the second
contains two \texttt{cascades} which in turn each contain a
\texttt{menu} again. To make sense of these menus we have to fill them
of course. And finally we have to put some functions behind all those
commands and what so ever or this would all be for naught.

\begin{code}
    varSize <- createTkVariable currentSize
    sr1 <- createMenuRadioButton pmc1m 
            [text "tiny (6x6)", value tinySize, 
             variable varSize]
    sr2 <- createMenuRadioButton pmc1m 
            [text "small (10x10)", value weeSize,
             variable varSize]
    sr3 <- createMenuRadioButton pmc1m 
            [text "normal (15x15)", value normalSize,
             variable varSize]
    sr4 <- createMenuRadioButton pmc1m 
            [text "large (20x20)", value bigSize,
             variable varSize]
    sr5 <- createMenuRadioButton pmc1m 
            [text "huge (25x25)", value hugeSize,
             variable varSize]

    varDiff <- createTkVariable (6:: Int)
    dr1 <- createMenuRadioButton pmc2m 
            [text "easy", value (8::Int),
             variable varDiff]
    dr2 <- createMenuRadioButton pmc2m 
            [text "normal", value (6::Int),
             variable varDiff]
    dr3 <- createMenuRadioButton pmc2m 
            [text "hard", value (4::Int),
             variable varDiff]
    dr4 <- createMenuRadioButton pmc2m 
            [text "nuts", value (3::Int),
             variable varDiff]
\end{code}
In the code above we do several new things. First we create a new
\texttt{Tk variable} named \emph{varSize} and a few lines later one
called \emph{varDiff}. These are necessary to remember things we do to
our GUI like in this case switching a button.  Our next step is to
create a \texttt{menu radio button}\footnote{ 
  A radio button is a button with several instances. Each of the
  instances is shown, can be selected and is bound to a different
  value but to the same variable. By selecting one of the instances
  the according value is asign to the variable. Because a variable can
  only have one value at a given time, only one of the instances can
  be selected.}
in each of the two submenus. The first \texttt{radio button} asigns
\texttt{tuples of Int} to the Variable \emph{varSize}, the actual
values are given in a couple of functions above in the code which can
be fully seen in the source.

As you we can see in the code above (in \texttt{main} and
\texttt{run}), \emph{currentSize} holds the value of \emph{normalSize}
and so by default the \texttt{radio button} is set to
\emph{normalSize}. Nearly the same happens to \emph{varDiff} and it's
\texttt{radio button} just without the hiding functions.

The values of \emph{varDiff} have to be explicitly casted because by
default \texttt{TkVariables} contain \texttt{Strings}. But still
nothing happens besides setting two variables.

\begin{code}
    restartClick <- clicked restb
    quitClick <- clicked quitb

    csr1 <- clicked sr1
    csr2 <- clicked sr2
    csr3 <- clicked sr3
    csr4 <- clicked sr4
    csr5 <- clicked sr5
\end{code}

This rather cryptic peace of codes allows us to bind the
\texttt{commands} and the size \texttt{radio button(s)} to a couple of
\texttt{events}. These will occure now whenever one of the buttons is
selected -- "`\texttt{clicked}"'.

\begin{figure}[h]
\begin{center}
\includegraphics[scale=0.6]{img/Screenshot02}
\caption{The hsMines main window with an open Prefs/Size menu}
\end{center}
\end{figure}

\subsection{The field}

To make all the decoration perfect, we need a little smiley atop the
playfield which can be used to restart the game.
\begin{code}
    sm <- newButton main [photo smSmileImg]
    startClick <- clicked sm

    pack sm [Side AtTop, PadY 20, PadX 20, Anchor North] 
\end{code}
So we create a \texttt{Button} called \emph{sm} (from \textbf{sm}iley
btw) and bind it to another \texttt{Event}. And because the GUI does
not automaticaly know where and when to place the \texttt{Button} we
have to tell it to \texttt{pack sm} \texttt{at} the \texttt{top} of
the \texttt{main} window, \texttt{pad} it \texttt{20} pixels wide in
any direction and keep it align to the upperside (which is
\texttt{North} on most maps). The \texttt{photo} command assigns an
\texttt{Image} to a \texttt{Container} that is fit to contain an
\texttt{Image} e.g. a \texttt{Button widget}. In this case we have a
collection of base64 encoded GIFs pasted into the code. This is
definitivly not the best way to use graphics, but it works for small
ones. As we will see later, there are also tiny \texttt{Images} for the
empty field and the numbers and not only the flag. The reason for that
is, that you can't overwrite a \texttt{photo} with some \texttt{text}.
We may do it internaly but it will not be shown. The workaround is to
have \texttt{Images} for all the numbers. On the other hand we have a
wide selection of colours for the numbers. Just try to find out wich
colour the '8' has\dots But back to the smiley.

Same as the \texttt{menu commands}, the \texttt{Button} is useless by
itself. To get things started we create an \texttt{IO Channel} named
\emph{restartCh}.  We can later at any given time send some
\texttt{IO} over this \texttt{Channel} to trigger an \texttt{Event}
which will be used to reinitialise the game field.

\begin{comment}
\begin{code}
    bfr <- newFrame main [width (cm 10)]
    size <- readTkVariable varSize
\end{code}
\end{comment}

\begin{code}
    restartCh <- newChannel
    allbuttons <- buttons bfr sm (receive restartCh) size
\end{code}

But before we can initialise any game field, we of course have to
create it. To contain the field we create a new \texttt{Frame} (a
\texttt{Container widget}) with a given \texttt{width}. The
\texttt{width} is just to have something to start with and will be
adjusted by the \texttt{packer} as needed. The \texttt{Frame} is
packed below the smiley button. They both are told to be at the top of
\emph{main}, but because only one of them can be there they're placed
below each other in packing order.

\begin{verbatim}
    bfr <- newFrame main [width (cm 10)]
\end{verbatim}
\begin{code}
    pack bfr [Side AtTop, PadX 15] 
\end{code}

Until now this was all very plain and straight. The \texttt{Buttons}
for the field are created in a more complex way. The function
\texttt{buttons}, the one called with the \texttt{IO Channel}, is
responsible to create a number of \texttt{Buttons} and asigns them to
\emph{allbuttons}. To pack them, we have to iterate through the whole
\texttt{list of Buttons}.

\begin{code}
    mapM_ (\(xy, b)-> grid b 
          [GridPos xy, GridPadX 1, GridPadY 1]) allbuttons
\end{code}
%\)
In this case a \texttt{grid packer} is used that puts all the
\texttt{Buttons} at exactly stated positions in a \texttt{grid}. So
let's for now assume, \texttt{buttons} returns a list of
\texttt{Buttons} that can be packed by the \texttt{grid packer}. If
the code compilation does not reach any point where the game is
started, the packed but uninitialised game field including the smiley
and all thing looks like this. To make a distinction between
uninitialised and initialised fields, we put a little star on
each \texttt{Button}.

\begin{figure}[h]
\begin{center}
\includegraphics[scale=0.3]{img/Screenshot03}
\caption{The uninitialised hsMines field}
\end{center}
\end{figure}

All that is left to do now in \texttt{run} is to initiate the game for
the first time. 

\begin{code}
    let start :: IO () 
        start = do diff <- readTkVariable varDiff
                   sendIO restartCh diff
                   
    -- start the menu handler
    stopmh<- spawnEvent 
     (forever (startClick >>> start
        +> quitClick >>> destroy htk
        +> choose [csr1, csr2, csr3, csr4, csr5] >>> 
             createMessageWin "Changes come into effect 
               after \"Restart\"." [] ))

    -- the restart handler (note no forever!)
    spawnEvent (restartClick >>> 
                  do stopmh
                     destroy main
                     nuSize <- readTkVariable varSize
                     run htk nuSize)

    -- start the game
    start

    -- wait for game to stop, then clear up the mess
    finishHTk
\end{code}

There is a bit more to clean up the window and such. We will see to
this now. Let us go through this step by step. \texttt{start} is a
function that returns an \texttt{empty IO} operation. Inside the
functions, the variable \emph{varDiff} which holds the games
difficulty is read and sent via the \texttt{IO Channel}
\emph{restartCh}. This will take effect in \texttt{buttons} and we'll
come to this later. But \texttt{start} is not the first thing to
happen. First, an \texttt{Event} is bound to \emph{stopmh} but is not
\texttt{spawned} instantly because haskell is evaluated lazyly and so
the evalutation will take place when \emph{stopmh} is realy needed -
which happens to be just in the next line. There another
\texttt{Event} is \texttt{spawned}.  This one immediately performes 4
functions. Now \emph{stopmh} is evaluated and the \texttt{Event} is
\texttt{spawned}. The second action is to \texttt{destroy} the
\emph{main} window. Step 3 and 4 create a new window with the actual
field size. This is the reason fpr the intervined function calls of
\texttt{run} and \texttt{main}.

The second \texttt{Event} thread, that is spawned in step 1 now waits
for a number of things to happen. Either the \emph{startClick}
\texttt{Event} occurs and \texttt{start} is executed or the
\emph{quitClick} \texttt{Event} occurs and the whole \HTk is
\texttt{destroyed} or one of the size selecting \texttt{radio buttons}
is clicked meaning that one of the five \emph{csr} \texttt{Events}
occurs which leads to the creation of a \texttt{message window}.

Finally, at the end of the \texttt{run} function \texttt{start} is
called and the game is started. Remember? The size of the game field
is sent via \emph{restartCh} and something will happen in
\texttt{buttons}.  Let's take a look at
%This is NO typo but intentionaly done.

\subsection{The \texttt{buttons} function}

This function has a rather complex signature.

\begin{code}
buttons :: Container par=> par-> Button-> Event Int
                           -> (Int, Int)
                           -> IO [((Int, Int), Button)]
\end{code}

For comparison lets have another look on how it's called:

\begin{verbatim}
    allbuttons <- buttons bfr sm (receive restartCh) size
\end{verbatim}

We have a class restriction on the first argument, \emph{par}, which
has to be a \texttt{Container}. \emph{par} happens to be just that, a
\texttt{Frame}. Lucky us. The next argument has to be a
\texttt{Button} as we can happily admit our smiley buttons \emph{sm}
is. The third argument has to be an \texttt{Event of Int}. This
\texttt{Event} occurs, when some \texttt{IO Int} is send via the
\texttt{IO Channel} \emph{restartCh} and is received by
\texttt{receive}. The last argument is a \texttt{2-tuple of Int} which
is the size of the game field. \emph{size} is an \texttt{Int} read
from the \texttt{TkVariable} \emph{varSize}. So when something is
asigned to \emph{allbuttons}, the function \texttt{buttons} is called
with a lot of intricate arguments.

When \texttt{buttons} has done it's work, it will return an \texttt{IO
  of a list of tuples of tuples of Int and a Button}. We can see that
it is simpler to examine the signature ourselfs than try to puzzle out
what we just read.

The code of \texttt{buttons} is simple at start.

\subsubsection{Create an array of buttons,\dots}

\begin{code}
buttons par sb startEv (size@(xmax, ymax)) =
  do buttons <- mapM (\xy-> 
       do b<- newButton par [photo starImg, relief Raised]
          return (xy, b)) [(x, y) | x <- [1.. xmax],
                                    y <- [1.. ymax]]
\end{code}

This code is executed no matter what \emph{startEv} might be! It
creates all the \texttt{Buttons} and paints a little star inside so
the playfield looks the way the Figure above shows it.
%Verweis auf Figure?
But there is more to happen in the \texttt{buttons} function!

\subsubsection{bind them\dots}

\begin{code}
     let bArr = array ((1,1), size) buttons
         getButtonClick b n xy = 
            do (click, _) <- bindSimple b 
                 (ButtonRelease (Just n))
               return (click >> return xy)
     leCl  <- mapM (\(xy, b)-> getButtonClick b 1 xy) 
                                 buttons
     riCl  <- mapM (\(xy, b)-> getButtonClick b 3 xy) 
                                 buttons
     press <- mapM (\(_, b)-> 
       do (cl, _)<- bindSimple b (ButtonPress Nothing)
          return cl) buttons

\end{code}

This looks rather complicated but does nothing more than what we did,
when we bound the smiley \texttt{Button} to the \emph{startClick}
\texttt{Event}. It's just that we bind the whole \texttt{Array of
  Buttons} we created via a slightly adjusted \texttt{Event}
(\texttt{getButtonClick b 1/3 xy})\footnote{ 
  As you could surely guess, 2 would be the modifier to get the center
  mouse button bound.}
to two \texttt{Events}. We also bind the \texttt{Button} to the
\emph{press} \texttt{Event}. This \texttt{Event} occurs whenever a
\texttt{Button} is pressed.  It's used to alter the smiley and because
it doesn't matter which \texttt{Button} is pressed, we don't keep the
coordinates.  Know the programm knows, whether a \texttt{Button} is
clicked and, when it's released, which \texttt{Button} it is and if it
was clicked with the left or the right mouse button.  Funny thing is
that if you use the middle mouse button to click a \texttt{Button},
the smiley will stay worried.

\subsubsection{and start\dots}

\begin{verbatim}
     spawnEvent start
     return buttons
\end{verbatim}

At the end, we \texttt{spawn} an \texttt{Event} called \emph{start}
and wait. But wait what for? We need to remember that an
\texttt{Event} is handed over to \texttt{buttons}. And this
\texttt{Event} is used to get the game running.

\begin{comment}
\begin{code}
     let
\end{code}
\end{comment}

\begin{code}
         start :: Event ()
         start = 
           startEv >>>= \d-> 
             do m <- createMines (snd (bounds bArr)) d
                sb # photo smSmileImg
                mapM_ (\b-> b # photo zeroImg >>= 
                                relief Raised) (elems bArr)
                sync (play m)
\end{code}

This says: If the \texttt{Event} \emph{start} occurs (by being
\texttt{spawned} for example) then \emph{startEv}, if it occurs,
executes some more code. Have a look on how we create mines later,
it's of no importance for the GUI. We map over all elements in the
\texttt{Button Array} \emph{bArr} and asign the \emph{zeroImg} to
their \texttt{photo}. Then we asign a new smiley to the \texttt{photo}
of \emph{sb} and \texttt{synchronize} the \emph{start} \texttt{Event}
to the \emph{play} \texttt{Event}. No new \texttt{Event} thread is
created, \emph{start} just changed into \emph{play}.

\subsubsection{to play\dots}

\begin{code}
         play :: Mines-> Event ()
         play m 
           = do r <- choose leCl >>>= open bArr m
                case r of Nothing -> always gameLost 
                            >> gameOver
                          Just nu -> if all (not.untouched)
                                            (elems nu) then 
                                        always gameWon 
                                           >> gameOver
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

\end{code}

Playing is easy. To play with a set \emph{m} of mines means to execute
these steps over and over again:

\begin{enumerate}
\item If the left mousebutton is pressed, execute \texttt{open} with
  the \texttt{Button Array} and the mines. If \texttt{Nothing} is
  left, you loose\footnote{ 
    Of course this is the case because \texttt{open} returns Nothing
    if we try to explore a mine.}
  If there are mines left, you play on. And if all fields are explored
  we won. Great, isn't it?
\item If the right mousebutton is pressed, execute \texttt{flag} with
  the \texttt{Button Array} and the mines. No evil may occur, just
  play on.
\item If a mouse button is pressed (remember, even the middle button
  counts), the smiley should look worried. Normaly another
  \texttt{Event} occurs so the smiley changes within a second but only
  if we pressed the left or right mouse button. This, of course, is a
  feature.
\item If things go awry, goto \emph{start} and wait for the
  \emph{startEv} \texttt{Event}.
\end{enumerate}

\begin{code}
         playOn :: Mines-> Event ()
         playOn m = do always (sb # photo smCoolImg)
                       play m       
\end{code}

\texttt{playOn} takes care of the smiley when we (de)flagged some field
or explored it.

\subsubsection{until it's over.}

\texttt{gameLost} and \texttt{gameWon} are just windows to open,
to tell you that you've lost or won. That also changes your smiley
into a freak.

\begin{code}
         gameLost :: IO ()
         gameLost = 
           do sb # photo smSadImg
              createAlertWin "*** BOOM!***\nYou lost." []
         gameWon :: IO ()
         gameWon = 
           do sb # photo smWinImg
              createMessageWin "You have won!" []

\end{code}

Only after you finished up the messenger, the game is realy over and
can be started again, the \verb§>>§ operator takes care of that.

\begin{code}
         gameOver :: Event ()
         gameOver = start 
                    +> 
                    (choose (leCl++ riCl) >> gameOver) 
\end{code}

\begin{comment}
\begin{code}
     spawnEvent start
     return buttons
\end{code}
\end{comment}

\texttt{gameOver} is the \texttt{Event} to take over. It leads you
back to the \emph{start} \texttt{Event}, waiting for the
\emph{startEv} \texttt{Event} to occur. If any of the ingame
\texttt{Events} still occur, because some dumbhead did not understand
the "`BOOM! You lost."' message or what so ever, the game is still
over and nothing changes.

That's it, anything else is just plain haskell. Naugh, you're right,
there is some tiny tidbits left. Nobody explained how the numbers show
up when a non mine field is explored, right? Okay, we'll come to that
now.

\subsection{Fuzzing around on the play field}

The simple part is leaving and taking flags.

\begin{comment}
\begin{code}
-- drop or retrieve a flag (mouse right-click)
\end{code}
\end{comment}


\begin{code}
flag :: Buttons-> Mines-> (Int, Int)-> IO Mines
flag b m xy = 
  case m!xy of
    Cleared _ -> return m
    s@(Unexplored{flagged= f})-> 
        do b!xy # (if not f then photo flagImg 
                   else photo zeroImg)
           return (m // [(xy, s{flagged= not f})])
\end{code}

The function takes the \texttt{Button Array}, the \texttt{Mines} and
the coordinates of the selected \texttt{Button}. If the the field is
\emph{Cleared} which means it was explored more early the
\texttt{Mines} are simply returned. If the field is
\texttt{Unexplored} and flagged we set it unflagged and asign the
\emph{zeroImg} to its \texttt{Button} and vice versa.

The more complex part is actually exploring the field.

\begin{comment}
\begin{code}
-- open up a field (mouse left-click)
-- returns Nothing, if we click on a hidden mine, the input if we 
-- click on a flagged field (without a mine), and peeks at the field
-- otherwise

-- Crimson: I switched the order of Flag and Mine because it sucks to 
-- accidently click a Flag and get killed... 
-- I also put the Cleared _ expression on top because I think this saves 
-- computation time. 
\end{code}
\end{comment}

\begin{code}
open :: Buttons-> Mines-> (Int, Int)-> IO (Maybe Mines)
open b m xy = 
  case m!xy of 
    Cleared _                  -> return (Just m)
    Unexplored {flagged= True} -> return (Just m)
    Unexplored {mine= True}    -> return Nothing
    _ -> peek b m [xy] >>= return. Just
\end{code}

The function takes the same arguments but it only may return
\texttt{IO Mines}.  If we try to explore an \texttt{Cleared} field,
nothing changes. If we try to explore an \texttt{Unexplored} field
which is flagged, we return the given argument and still nothing
changes. If we try to open an \texttt{Unexplored} field which is a
mine the function returns \texttt{Nothing} wich leads to
\texttt{gameLost} in \texttt{buttons}. And at last, when every other
case is weeded out, we \texttt{peek} inside an \texttt{Unexplored},
nonflagged nomine field.

\begin{comment}
\begin{code}
-- Peek at a list of fields, and count the number of
-- adjacent mines. If there are none, we recursively peek at all the
-- adjacent fields, which are
-- a. not already cleared, and
-- b. not on our list of fields to peek at
-- Precondition: all fields in the list are untouched.
\end{code}
\end{comment}

\begin{code}
peek :: Buttons-> Mines-> [(Int, Int)]-> IO Mines
peek b m [] = return m
peek b m (xy:rest) =
   let adjMines :: Int
       adjMines = sum (map (mines. (m !)) (adjacents m xy))
       nu       = m // [(xy, Cleared adjMines)]
   in do (b!xy)# photo (getImg adjMines) >>= relief Flat
         if adjMines == 0 then 
            peek b nu (rest `union` 
                      (filter (untouched. (m !))
                              (adjacents m xy)))
            else peek b nu rest
\end{code}

\texttt{peek} takes a list of coordinates to check. Initially this
should be exactly one coordinate respectivly the one we try to explore
right now. We use \texttt{adjMines} the get the number of
\texttt{Mines} around the field to explore wich is a number from 0 to
8. We asign the corresponding \texttt{Image} to the \texttt{Button's
  photo} and change its \texttt{relief} to \texttt{Flat}. In case the
number is not 0, this would just be it because now the \texttt{list of
  coordinates} would be empty. But to make life more comfortable we
will now use the computer for its main purpose: To accomplish stupid
tasks for us. Because if the number of \texttt{Mines} in the adjacent
fields is 0 we can savely explore any of these. So now \emph{rest},
the list of coordinates still to explore (an empty \texttt{list} right
now) is \texttt{unified} with the coordinates of the adjecent fields
which are still \texttt{Unexplored}. If we forgot to apply this
\texttt{filter} the exploration would go on forever exploring the same
fields over and over again. But if it's done right the programm
explores all save fields thereby creating number rimmed empty areas on
the playfield.

\begin{figure}[h]
\begin{center}
\includegraphics[scale=0.3]{img/Screenshot04}
\caption{The hsMines field after a single lucky Click}
\end{center}
\end{figure}

And just in case you don't believe this for real I finished the game
;-)

\begin{figure}[h]
\begin{center}
\includegraphics[scale=0.3]{img/Screenshot05}
\caption{The same field a few lucky Clicks later}
\end{center}
\end{figure}


The excurse through the HsMInes source is over for now. There is still
a couple of interessting functions left one can explore on it's own.
Have fun.


%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "intro"
%%% End: 

