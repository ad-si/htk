
\section{The HsMines Game}

\textit{Contributed by Christoph Grimmer
  \texttt{<crimson@informatik.uni-bremen.de>}}

As an example for more complex GUI programming, we will now develop a
GUI for a Minesweeper like game called hsMines. Just in case you do
not know Minesweeper or any of its many clones I'll give a short
overview.

\subsection{What's that game?}
Playing Minesweeper you have a grid of about 15 by 15 similar fields.
Hidden inside these fields could be a mine or just an empty field --
you wont know until you click at the field and are eventually shred to
pieces by some nasty mine. The goal of the game is to find all the mines
and mark them with tiny flags --- a nearly impossible task. To make the
game any fun you are told the exact amount of mines around the field
you just opened. If this number is 0, it's safe to explore all the
adjacent fields. And because this is a stupid task, the machine does it
for you. If you manage to explore all empty fields you win, if you
find one of the mines, you lose, best time gets the highscore.  Simple
as that.

This is not a course in Haskell programming I will not spent many
words on the games code itself and will come straight to the very
heart of this section, the hsMines GUI. You can find the source for
the game (and this text) in \texttt{htk/doc/intro/MainhsMines.lhs}, in
case you are curious.

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
                          mine    :: Bool }

-- A field is still untouched if it is neither cleared nor flagged
untouched :: State-> Bool
untouched (Cleared _) = False
untouched (Unexplored{flagged = f}) = not f

-- Counts the number of mines on a field
mines :: State-> Int
mines (Unexplored{mine= True}) = 1
mines _                        = 0

-- Similary, the number of flags
flags :: State-> Int
flags (Unexplored{flagged= True}) = 1
flags _                           = 0

-- Our playing field: an array of states, and the button handlers for 
-- them. (We keep them separate, since the state will change,
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
zeroImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAP///2AAABAAAAwAAACwYQA5fgAGBABAQHwE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEAEEAACsAAEAAABT4NLcvZRAUEEAICDYnBOyAAAcEAEBAAAOc+AA4LwAUFAAICCAEJzYAgBQABAgAQFz4BAIvAAAUAAAIAPcE+DkALwwAFAgACKT4J/QvgP8UBL8IQBQnCLeANxAEFEBACNTowPM28f8U/78Iv2fEBPfyAAf/AEC/AJgEYTUAfiBhADZ+ABQEAAhAAFzcAALAAACyAAAAADQEAPQAAP8AAL8AACQU4gC35QAQGgjw/1jyAAT/kQRnAPT3JP8Hkf0EAAYAAJj4aDUv8xQU/wgIvyAnATaAAFwcMAI3ZQAUEND0FCvytwb/EEC/QLAEAGwAABQAAMBhOE1+8isE/0BAvwAATQAAqAAAGQAAQBQEVLcAQRAAK0AAQPgULC+38hQQ/whAv1gEVPQAQf8AK78AQCH4AEAvAAgUAAgIAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4wPQvTf8UK/iwAMNsABQUAABk1AA48gAU/wAIvw6+A4iCAAQEAEy+FAKCtyAEEANAQMgUwPS3Tf8QK79AQMwymPSNNf8EFL9ACPgUTMO38gAplgAA7AAABxf4A4IvAAcUAPgU1AEEAgD4AAAvAADcAAE4AAC3twAQENQEBNAU+PS3L/8QFMwEBIj4+PQvL/8UFL8ICFGMJ4LzgAf/BAi/QPjqcMO4OBQfFAhACBBUcABB8gAr/wBAvxB7BABgAAAQAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIHQABCBxIsKDBgwgTKlzIsKHDhxAjSpxIsaLFgQEBADs="])

{-# NOINLINE zeroImg #-}

oneImg :: Image
oneImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAEgO9HFE9oVe+Jp6+cKv+9fK/evl/v///3wE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAICwAaxsKBUUBggIQAAoXAA4OA6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAIAU5Ky3NhUQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAIAUCKy3ABUQAAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QIDqNKy4OBUfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIOwAPCBxIsKDBgwgTJiQwIIBCgQIASHx4oIABAgAoCsSo8QBHjR8phkxYgCEAAgQURpQosUDHlzBjGgwIADs=" ])

{-# NOINLINE oneImg #-}

twoImg :: Image
twoImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAD2eJVOpPn6+bpTJhqnUnr7fttTqz+r05////2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAJCwAcZsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAJAU5Ma3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAJAUCMa3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QJDqNMa4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIRwARCBxIsKDBgwgTIjwwAACAAQcUNiRAIMAAhQEICCQAQCFBAQE8ChQAoIDHAyRNKjwQoKRHAy0JFOCYsIDDmx1F6tzJc2dAADs=" ])

{-# NOINLINE twoImg #-}

threeImg :: Image
threeImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAPQdFvU3MPZQSvhoZPmCfvqbmPu0sf3NzP7m5f///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvADCwAbNsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EADAU5LO3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUADAUCLO3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QDDqNLO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAITAATCBxIsKDBgwgTIkRAAAAAAQgUDghQwEAAAQoRRExgAIBCgQcsDviYIIDDAyQFDgCwkSQAAwkJoESwsqVBkw5ffjTA02bKn0ARBgQAOw==" ])

{-# NOINLINE threeImg #-}

fourImg :: Image
fourImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAACk0k0FLn1lhq3B4t4mPw7i729DS5+jp8////2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvACiwAa5sKBUUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EACgU5K63NhUQFAhACAApvgAAggAABAAAQBf4A4IvAAcUACgUCK63ABUQAAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QCjqNK64OBUfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIQQARCBxIsKDBgwgTKkRAYICBhQgKAABQAGIAARQXEgBwIGPCjgQQeESIsYDEkAknqgQA8cBJiBsBBIBIs6ZNmwEBADs=" ])

{-# NOINLINE fourImg #-}

fiveImg :: Image
fiveImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAJYTE6IuLq1ISLliYsV9fdCWltywsOjLy/Pl5f///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAKCwAd9sKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAKAU5N+3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAKAUCN+3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QKDqNN+4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAISwATCBxIsKDBgwgTIjwwoOGAAgoNAHBoICIAhQQlBgAgAIHCAwAIGBAQAONABAAqmkyQMqEAiAkGAPCIUACAmy0/GjBAc6XPnwoDAgA7" ])

{-# NOINLINE fiveImg #-}

sixImg :: Image
sixImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAACdoIEB5OW+aaoishLfNtdDezufu5////3wE3GAAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAFCwAchsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAFAU5Mi3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAFAUCMi3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QFDqNMi4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAISwAPCBxIsKDBgwgTJjQgAAAAAgoLAAhAQIABhQECKBxIAEABAgU2DpjoUIDCkSY7hkTY8eKBhwsBpATgkqVDmRsPGACZs6fPnwcCAgA7" ])

{-# NOINLINE sixImg #-}

sevenImg :: Image
sevenImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAALwxrstfwNJ2ydqN0uGk2+m65PDS7fjo9v///2AAwBAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhADZ+ABQEAAhAAFzcAwLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEBwhAQFwcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAOCwAeNsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAOAU5OO3NhQQFAhACAApvgAAggAABAAAQBf4A4IvAAcUAOAUCOO3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QODqNOO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIOwARCBxIsKDBgwgTIhTAsKFCAAEYBgCg0MABgQICKCQIgMBGgQUAXPyY8aPAjiZDjtw4QKPJlzBjfgwIADs=" ])

{-# NOINLINE sevenImg #-}

eightImg :: Image
eightImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAABi6rzLCuEzJwWXRyn/Z05ng3LLo5Mzw7eb39v///xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQEwEEATSsAK0AAQBT4ALcvABAUAEAIADYnVOyA8gcE/0BAvwNgAwA4AAAUAAAIACAEFDYAtxQAEAgAQFz4wAIvTQAUKwAIQPcEmDkANQwAFAgACKT4zPQv8f8U/78IvxQnlreA7BAEB0BAQNToA/M2AP8UAGfEVPfy8gf//0C/v5gEAjUAABQAACBhYTZ+fhQEBAhAQFzcAgLAAACyAAAAADQDFPQAt/8AEL8AQCQg7AA28QAU/wAIvwj8p1gH9wQABwSsmPQ4Nf8UFL8ICP0EVCsA8gYA/0AAv5j4AjUvABQUAAgIACAnZzaA9xQEB1wcmAI3NQAUFAAICND0FCvytwb/EEC/QLAEjGwA8hQA/wgAv8Bh0k1+OSsEBgAAmAAANQAAFAAACBQEVLcA8hAA//gUAi+3ABQQAAhAAFgEsPQAOf8ABiH4AEAvAAgUAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4AvQvAFCwAchsKBQUBggIQAAoLAA4Nw6+AIiCAAQEAEBAAEy+BAKCACAEAANAAMgUA/S3AP8QAMwyAPSNAP8EAFAU+Mi3LxQQFAhACAApJwAAgAAABAAAQBf4A4IvAAcUAFAUCMi3AAEEVAAA8gAA/wAAvwD4AAAvAACgAAE4AAC3AAAQANQEBPQAAP8AAL8AANAU+PS3L/8QFL9ACMwEBIj4+PQvL1GMJ4LzgAf/BAi/QFDqNMi4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAIUAATCBxIsKDBgwgTJiwQAIAABAoLAChgIIAAhQMGCDQAIGIAiAMuKhQAoOQBhRwpCgiAUaNAAAYSEviYgACAkwgRNCxZQOFGAxB9Ch1KdGBAADs=" ])

{-# NOINLINE eightImg #-}


-- for testing purpose only

starImg :: Image
starImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhEAAQAPcAAAAAAB0dHTk5OVVVVXJyco6OjqqqqsfHx+Pj4////xAAsggAALT43PMvwP8Usr8IAGwn3PSAwP8Esr9AAFQE+UEAySsAFEAACBT4CbcvyhAUFEAICDYnBOyAAAcEAEBAAAOc+AA4LwAUFAAICCAEJzYAgBQABAgAQFz4BAIvAAAUAAAIAPcE+DkALwwAFAgACKT4J/QvgP8UBL8IQBQnCLeANxAEFEBACNTowPM28f8U/78Iv2fEBPfyAAf/AEC/AJgEYTUAfiBhADZ+ABQEAAhAAFzcAALAAACyADQEAPQAAP8AAL8AACQUVAC3QQAQKwBAQAjwBFjyAAT/AARnqPT3Yv8HB79ACP0EKCsA8gYA/0AAv5j4vjUvYhQUBwgICCAn2TaAyRQEFAhACFwcCQI3ytD0FCvytwb/EEC/QLAEAGwAABQAAMBhOE1+8isE/0BAvwAA0wAAKAAABgAAQBQEBLcAABAAAEAAAPgUOC+38hQQ/whAv1gEACH4AEAvAAgUAAgIAJw4AEfzAAz/AAi/AAAcBACDAAAEAABAAMz4FPQvt/8UEPiwAcNsKBQUBggIQABk+A6+AIiCAAQEAEy+HAKCg+AEBAJAQMgUA/S3AP8QAMwyAPSNAP8EAPgU0MO3NhQQFAApvgAAggAABBf4A4IvAAcUAPgUCMO3ABQQAAEEAAD4AAAvAADcAAE4AAC3AAAQANQEBNAU+PS3L/8QFMwEBIj4+PQvL/8UFL8ICFGMJ4LzgAf/BAi/QPjqcMO4OBQfFBBUcABB8gAr/wBAvxB7BABgAAAMAACcYQDzfgD/BAC/QHhW3PfFwP8fsgC03ADzwAD/sgC/AAB83ABgwAAQsgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAEAAQAAAITAATCBxIsKDBgwgTKlxY0MAAhgIdLkQQ8WECBBQLHggggAABAQUEACiA8ECBAAAIGFi4cQDJhQEMIAhwQCEBiwYCKKw5kCfEn0ATBgQAOw==" ])

{-# NOINLINE starImg #-}

\end{code}

These are the bitmaps for the graphics, taken from gnomines. 

\begin{code}

flagImg :: Image
flagImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhDAALAPEAAAAAAP4CBLLA3AAAACwAAAAADAALAAACGpQfcCvhypyERlqKLstpL4BtoHeMHohi6QoUADs=" ])

{-# NOINLINE flagImg #-}

smCoolImg :: Image
smCoolImg = unsafePerformIO (newImage [imgData GIF "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"])

{-# NOINLINE smCoolImg #-}


smSadImg :: Image
smSadImg = unsafePerformIO (newImage [imgData GIF "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"])

{-# NOINLINE smSadImg #-}


smSmileImg :: Image
smSmileImg = unsafePerformIO (newImage [imgData GIF "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" ])

{-# NOINLINE smSmileImg #-}


smWorriedImg :: Image
smWorriedImg = unsafePerformIO (newImage [imgData GIF "R0lGODdhIAAgAPcAALLA3MPCvKmpqnd2clZWVEVFRDs6NGZiNKqeQOjSVOjOPd7MRK6eRHpyPFpWPGpqbJ6enLCwsM7OzNXVz6KijHJydExMSjQ0MRwcDCIeBDYyHFpODI6CLIh6NHpuJExGIGpmVFJORCQiEBcWDhYSCB0dHIB+dCQkJCsrLF1dXI5+LIqKjKKefFZSLOzWLPnbN2ZeLPPaVqmKGtW+JPneJdjCNKKOPMauVPTeZMCySD02EW5qTPriKlFNNMqqTNKqLOrejPXWOPbaJuzWQNrGTOrWdPXjZ/XiR/riPvHaOt7GRNa+RN2+JNW4HtKyNL62jN7atPvrcvrmVPXqn/bjhOTDLN7CKN25LcuuN8KiRNDMsv7ujP7ymMa6ZGZeFGZaDMayH1JOLKaeYNKyKcunLb6eLLisbPrunI6GTA0NBIp+FL6qJPbmnMqiLLyaIbaSKOLanPXSOfvugSsmDKeWL+7aLPDigNKuJMOeMvXWJPnsk+rcXMu6N+bWWNvKVNrOhO7SVOrKJuPCI8umIrGOJbCeaPDOOvrmNPrqVM7CZMWzOOTGIsamLLqWH7KKHKqSTPLOLP31tPLqsNbKZKiQFKGOHMaqHNy4I6aGOO7KOPLSJv76zOLWfIZ2FFxSFLKaFOK+IdeyKbSWHKF+JN7OZP32yX5yDE5GDJKCGM2qIa6GJJp6NOrGROrFMO7OJu7SJO7mtHBiEJ6KFJx2IOLGVL6ycNKqJMShIbySIKB6IJqCQN7KdOW+L6KSJJJ+FMCmGKGCGJ2MYuK6LMWZJJZzHq6mjNauLOrGIfXeV+7SOcyvHKiCIKmGHYtmGc66dN7GHJqHIUo+D0Q6DJBuFJp+FJBzFZh6GJZuGNbStMKuPDYuDIhyFH1jFIdrFI9qHbKSHHZdFMqiJJB0PJqCFIRjFHtdFLq6pIpiFMKudLaKJKZ+HraiZLaujAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAIAAgAAAI/gABCBQYQMAAAgUMGDiAIIECBQsYNHBAoOIDCBECSJgwYSAAChUsVDB4AQOGDBo2cOjg4QOIECJGkChxocADExE6Dixw4cQDARUulBh64gSKowYqpEAxlCiKEwE8AkhB4MQFCwZOEDWqQsXTC0+LGkWxwoLUqSw+iNBaQuyJFi5eOGDrFgUMCinOmogh48PMtkYvzKBBo0ZWrUWP2rhh4iyOFy9ywNBhtcAOBIR50MjR4ymKCz06+PhxQyqQIJCFDCFCpIiRI0g0ExaSxMUQBUqWMGnixMeTgVAgv6AxnMaRKFGk8JBNmIaRKVQSVLFyBcuPLFoEFgmCergQJFG2/nDp4uULmOVWGoQRA11BlSZjyJQxI5A7dyE0hBzZcoYLmjRpqLHcGiSkEcYUbLjHhBNtuPHGBHDEwV1qNPAgBxdciDHHHHTU4QIYOpDQABt2uHfFHT7g8cYTRUiYRx5CxEiDFFHosQcffPSxhx8zrHHDH1QAEoggTIwxSBmEFAKIIS6+gF9mhxyCiBxyAJGIImDMkGUgi1A3BiONOPIIk5AEAaOMsh1xYSSSTJIDHZRUYgkTRF5yB5iYYJJJJnFoEseLMtJQRx9nbLJJJJzk0IknXnzShBWghPIDHqI4MsqemWiiyZn4eUiKJKWUEgkQYJhyyimoPMpEKKkgqcoq/qy0kokrkGjyypk0KOAHHLBIYiMqX8TiiSxNMGEnI24QMsostLQiayCabtrpAomYUUsOlWwQCyWyWALKJU3YcgsuquSiyy688FJFIIG48sqtedRRhx9v9uJLJ78Icokg36ZyS7LAzBLMH1cIw4sg7Lri7qZ1JJFADXzMYIXCi4DSxC/DNEJIucQUM4ExlwgDiiDHtOtuEMgYgUxtybwy5C3KpCKKLMsws2wzHTljTCghj8xuMjHgsEcMQxjyDDRfRCPNNG5QU0011sxyDRoCYfODMSBfAgooiygASALZ0BGLNiNkMEc021ADzTTcdHMNMd5EJZAzZNhyB89aV4FKxTQkSKPDF9+EEjMlMlTDDTjNeNNMMFL5EM4gtkR+9xzgyKDMILd8Iso3lABDzOGINyPOWVrgMczj4dwSzi+3NCKKDOM8/XQ35IADDjeJyy1V6cMM40bGhMgAjDXVdNMNN9yUEws45ZRDjjfmnDWQFlkMgwshhDADzNNsI488Oc6fI47u0guETjqOOLIMMOrMUs0205BDTjPndHMO4+WTvs4y6vQ/yyzEqAYxpqG4YJAvf1KZwBMYgAl1EONtowgGO3RSvoAAADs=" ])

{-# NOINLINE smWorriedImg #-}

smWinImg :: Image
smWinImg = unsafePerformIO (newImage [imgData GIF "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"])

{-# NOINLINE smWinImg #-}

\end{code}
\end{comment}

\subsection{In the beginning is the Window}
As we have read, it all starts with a window. And since we are running
compiled code here, it's created in the \texttt{main} function.

\begin{code}
main = 
 do htk<- initHTk [withdrawMainWin]
    run htk normalSize 

run :: HTk-> (Int, Int)-> IO ()
run htk currentSize = 
 do main <- createToplevel [text "hsMines"]
\end{code}

Having only this, the GUI would look rather boring, but it's not bad
for just a few lines of code, isn't it? It looks a bit awkward but we
will see later that this is needed to make the field resizeable. If
this were all the program, we would need a line 
\begin{xcode}
    finishHTk
\end{xcode}
to clean up everything and finish. With that, our program would like
shown in Fig.~\ref{fig:hsMines-initial}.

%% The last line is necessary to clean up all we've kicked loose so far.

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=4.2cm]{img/hsMines/screenshot01}
\caption{The hsMines window, initial state.}
\label{fig:hsMines-initial}
\end{center}
\end{figure}

\subsection{Menus}

Let's start start building the interface, then. Every interface needs
menus, so let's create some.

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

Let's go through this step by step.  In line 1 we create a menu inside
\emph{main} (our window) that was created in \texttt{run} above. In
line 2 we tell \HTk to attach this new \texttt{menu} we call
\emph{menubar} to be the \texttt{menu} of \emph{main}.

In line 3 (we will not count the empty lines), we create our first
pulldown menu --- that is what actually is normaly called a menu. This
pulldown menu is created inside \emph{menubar}, is called \emph{fm}
and has the charming text `File'. To fill this menu empty, we create
menu entries, called menu commands inside \emph{fm}; a menu command is
the simplest form of menu item which you can just select (or not). So
by now we have \emph{restb} and \emph{quitb} inside \emph{fm} inside
\emph{menubar} inside \emph{main}.

Besides restarting and quitting, we will need to put some more
functionality into our menu. As we read
above, the game grid will be resizeable so we will need a
`Preferences' menu to have these commands in.

We create a second pulldown menu in \emph{menubar} called
\emph{pm}. In this menu we nest to submenus (called menu
  cascades). Each of the cascades has a name, \emph{pmc1} and
\emph{pmc2}, and a text configuration set to its title. The next
step is a bit tricky. One would now expect to fill the cascades
directly with some commands. But the \texttt{menu cascades} are only
\texttt{Containers} holding other menus, so we have to create
another two menus inside \emph{main} and assign them to the two
cascades.  These two menus are called \emph{pmc1m} and \emph{pmc2m}
which should be an abreviation for preference menu cascade first menu
(and second respectively).

By now we have a menu bar which holds two pulldown menus. The first
contains two commands, the second contains two cascades which in turn
each contain a menu again. To make sense of these two submenus we have
to fill them of course. And finally we have to put some functions
behind all those commands or this would all be for naught.

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

\begin{wrapfigure}[17]{l}{7cm}
  \begin{center}
    \includegraphics[width=6cm]{img/hsMines/screenshot02}
    \caption{The hsMines main window with an open menu}
    \label{fig:hsMine-menu}
  \end{center}
\end{wrapfigure}
In the code above we do several new things. First we create two Tk
variables called \emph{varSize} and \emph{varDiff}. These are
necessary to hold the state of the GUI, in this case the user's
selection.  Our next step is to create a menu radio button in each of
the two submenus. The first radio button assigns pairs of integers to
the TkVariable \emph{varSize}, the actual values are as indicated in
the text configuration and given in a couple of functions above in the
code which can be fully seen in the source.
Fig.~\ref{fig:hsMines-menu} shows the size submenu of the preferences.

As you we can see in the code above (in \texttt{main} and
\texttt{run}), \emph{currentSize} holds the value of \emph{normalSize}
and so by default the radio button is set to
\emph{normalSize}. Nearly the same happens with \emph{varDiff} and its
radio button.

Note that we have to resolve the overloading of the numeric constants
by type annotations, because TkVariables can hold all instances of the
class \texttt{GUIValue}; when we just write \texttt{value 3}, this
might also be e.g. the floating point number $3$.

But something should happen we click those menus, so we need to bind
them to some events:

\begin{code}
    restartClick <- clicked restb
    quitClick <- clicked quitb

    csr1 <- clicked sr1
    csr2 <- clicked sr2
    csr3 <- clicked sr3
    csr4 <- clicked sr4
    csr5 <- clicked sr5
\end{code}

This binds the commands and the size radio button(s) to a couple of
\texttt{events}, which will occur whenever one of the buttons is
selected (i.e. clicked). Note that we do not bind anything to the
difficulty submenu. This is because we are actually not interested in
the user selecting a new difficulty during the game, we just read out
the value set by the user every time we restart a game. (This is not
very polite --- it might be better to inform the user of this when he
changes the selection, but this is just a small demonstration program.) 

\subsection{The field}

To make all the decoration perfect, we need a little smiley atop the
playfield which can be used to restart the game.
\begin{code}
    sm <- newButton main [photo smSmileImg]
    startClick <- clicked sm
    
    pack sm [Side AtTop, PadY 20, PadX 20]
\end{code}
We create a \texttt{Button} called \emph{sm} (from \textbf{sm}iley
btw), bind it to an event, and pack it at the top of the \texttt{main}
window, padding it 20 pixels wide in both directions. The \texttt{photo}
configuration assigns an image to any widget that can hold one, such
as a button. In this case we have a collection of base64 encoded GIFs
directly in the code (not shown here); \footnote{The images have been
  taken from gnomines (the GNOME minesweeper clone), where they are
  attributed to \texttt{tigert} (Tuomas Kuosmanen).}
this has the advantages of making the code stand-alone. As we will see
later, there are also tiny \texttt{Images} for the empty field and the
numbers and not only the flag; the main reason for this is to that all
buttons should always have precisely the same size. We further have a
wide selection of colours for the numbers. Just try to find out wich
colour the 8 has\dots But back to the smiley.

Same as the \texttt{menu commands}, the \texttt{Button} is useless by
itself. To get things started we create a channel named
\emph{restartCh}. We will set things up such that sending something
(anything, really) over the
channel will restart the game.

\begin{code}
    restartCh <- newChannel

    bfr <- newFrame main [width (cm 10)]
\end{code}

But before we can initialise the game field, we of course have to
create it. To contain the field we create a new a frame with a given
width. The width is just to have something to start with and will be
adjusted by the packer as needed. The frame is packed below the smiley
button. They both are told to be at the top of \emph{main}, but
because it is packed after the button it is placed below.

\begin{code}
    pack bfr [Side AtTop, PadX 15] 
\end{code}

Until now this was all very plain and straight. The buttons for the
field are created in a more complex way. The function \texttt{buttons}
is used to create a number of buttons along with their position, and
assigns them to \emph{allbuttons} (i.e. \emph{allbuttons} has type
\texttt{[((Int, Int), Button)]}. To pack them, we have to iterate
through the whole list using \verb!mapM_!. And in order to have all
buttons appear on the screen at once, and not one by one, we wrap
\texttt{delayWish} around this.

\begin{code}
    size <- readTkVariable varSize
    allbuttons <- buttons bfr sm (receive restartCh) size
    delayWish $ mapM_ (\(xy, b)-> grid b 
          [GridPos xy, GridPadX 1, GridPadY 1]) allbuttons
\end{code}

\begin{wrapfigure}[15]{l}{6cm}
\begin{center}
\includegraphics[width=5cm]{img/hsMines/screenshot03}
\caption{The uninitialised hsMines field}
\label{fig:hsMines-uninitialised}
\end{center}
\end{wrapfigure}
In this case the grid packer is used to put all the
buttons at exactly stated positions in a grid. If
the code compilation does not reach any point where the game is
started, the packed but uninitialised game field including the smiley
and all thing looks like this. To make a distinction between
uninitialised and initialised fields, we put a little star on
each \texttt{Button}. The result is shown in
Fig.~\ref{fig:hsMines-uninitialised}. \\[3ex]

All that is left to do now in \texttt{run} is to initiate the game for
the first time. The actual playing happens in the event handling of
the buttons. \\[3ex]

\begin{code}
    let start :: IO () 
        start = do diff <- readTkVariable varDiff
                   sendIO restartCh diff
                   
    -- start the menu handler
    stopmh<- spawnEvent (forever
          (startClick >>> start
        +> quitClick >>> destroy htk
        +> choose [csr1, csr2, csr3, csr4, csr5] >>> 
             createMessageWin "Changes come into effect 
               after \"Restart\"." []))

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

There is a bit more to clean up the window and such; let us go through
this step by step. \texttt{start} is a function that reads the
variable \emph{varDiff} and sends the value over the channel
\texttt{restatCh}. This has the effect of restarting the game (see
\texttt{buttons} below, the event of receiving something on that
channel restarts the game).

First, we build a composed event that handles the menu buttons:
clicking the smiley (event \texttt{startClick}) should restart the game,
and selecting the quit menu (event \texttt{quitClick}) should finish the
game by destroying the main window. Selecting a new size
(\texttt{csr1} to \texttt{csr5}) should not do anything immediately,
but we are polity and inform users that they have to restart the
game--- clicking just the smiley is not enough!)

On the other hand, a separate event handles the restart. This is
because if we restart we destroy the main window, and start the game
again from the top. From the interface design point, this is bad
design for two reasons: firstly, it makes the interface very jittery
with windows unecessary opening and closing and what not, and
secondly, there is the awkward handling of having to restart the game
with the menu if you want to change the size. 

\subsection{The \texttt{buttons} function}

This function has a rather complex signature.

\begin{code}
buttons :: Container par=> par-> Button-> Event Int
                           -> (Int, Int)
                           -> IO [((Int, Int), Button)]
\end{code}

For reference recall how it was called:

\begin{xcode}
    allbuttons <- buttons bfr sm (receive restartCh) size
\end{xcode}

We have a class constraint on the first argument, \emph{par}, which
has to be container. \emph{par} happens to be just that, a
\texttt{Frame}. Lucky us. The next argument has to be a
button as we can happily admit our smiley button \emph{sm}
is. The third argument has to be an event, more precisely
\texttt{Event Int}. This event signals that the game should be
restarted with the difficulty level given; in the call, this is the
event which signals receiving something in the \emph{restartCh}
channel (recall from above how we send something along this
channel to restart the game). 

\texttt{buttons} creates all the buttons, and returns a list of pairs,
where each of these pairs contains the coordinates (as a pair of
integers) and the button. We can see that it is simpler to examine the
signature ourself than try to puzzle out what we just read.

The code of \texttt{buttons} is simple at start.

\subsubsection{Creating an array of buttons}

\begin{code}
buttons par sb startEv (size@(xmax, ymax)) =
  do buttons <- mapM (\xy-> 
       do b<- newButton par [photo starImg, relief Raised]
          return (xy, b)) [(x, y) | x <- [1.. xmax],
                                    y <- [1.. ymax]]
\end{code}

This code creates all the \texttt{Buttons} and paints a little star
inside so the playfield looks as shown in
Fig.~\ref{fig:hsMines-uninitialised}, using standard list
comprehension and map for monads. But there is more to happen in the
\texttt{buttons} function!

\subsubsection{Binding the buttons}

\begin{code}
     let bArr = array ((1,1), size) buttons
         getButtonRelease b n xy = 
            do (click, _) <- bindSimple b 
                              (ButtonRelease (Just n))
               return (click >> return xy)
     leCl <- mapM (\(xy, b)-> getButtonRelease b 1 xy) 
                                 buttons
     riCl <- mapM (\(xy, b)-> getButtonRelease b 3 xy) 
                                 buttons
     press <- mapM (\(_, b)-> 
       do (cl, _)<- bindSimple b (ButtonPress Nothing)
          return cl) buttons

\end{code}

This looks rather complicated but does nothing more than what we did
above when we bound the smiley button to the event \emph{startClick}.
First, we arrange our buttons in an array so we can later on refer to
the button at position $(x,y)$ easier. Then we bind three events to
each of the buttons: one for releasing the first (left) button, one
for releasing the third (right) button, one for pressing any button.
The left and right clicks are of type \texttt{Event (Int, Int)},
because we will later on have refer to the coordinates of a button
being released.

\subsubsection{Starting the game}

We now define three events (and a couple of auxiliary functions) which
encode the main logic of the game. The first one starts the game, the
third and second play the game. Then, if we synchronise on the start
event, we set the game in motion and in effect wait until it is over.

All of these definitions are local to \texttt{buttons} and use the
declarations from above, such as \emph{leCl}, \emph{riCl} and
\emph{bArr}. 

\begin{code}
     let start :: Event ()
         start = 
           startEv >>>= \d-> 
             do m <- createMines (snd (bounds bArr)) d
                sb # photo smSmileImg
                mapM_ (\b-> b # photo zeroImg >>= 
                                relief Raised) (elems bArr)
                sync (play m)
\end{code}

This says: after the start event (the argument of which is $d$ here,
the difficulty level), we create the mines on the playing field, make
the smiley smile, and fill all buttons with the zero image. Have a
look at how we create the mines later, it is of no importance for the
GUI. Then we play.

\subsubsection{Playing the game}

\begin{code}
         play :: Mines-> Event ()
         play m 
           = do r <- choose leCl >>>= open bArr m
                case r of Nothing -> always gameLost 
                            >> gameOver
                          Just nu -> playOn nu
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

Playing is easy. The \texttt{Mines} datatype needs not to be explained
right now, suffice it to say that it models the state of the playing
field (including the mines, but also keeping track of which fields
have been explored or flagged). To play with a set \emph{m} of mines
means to execute these steps over and over again:

\begin{itemize}
\item If the left mousebutton is released, call \texttt{open} with the
  button array, the mines and the position of the field we want to
  open (note clever $\eta$-reduced notation). If \texttt{open} returns
  \texttt{Nothing}, we lose; otherwise, we play on with the new
  playing field. 
\item If the right mousebutton is released, call \texttt{flag} with
  the button array, the mines and the position. No evil may occur,
  just play on.
\item If a mouse button is pressed (remember, even the middle button
  counts), the smiley should look worried. Normally any of the two
  release events above will occur within a short while so the smiley
  changes back, but if we press the middle button, the smiley stays
  worried. We consider this a feature, and in the best academic
  tradition leave it to the reader to come up with a solution.
\item On the other, if a start event occurs, restart the game; this
  can happen e.g. if the users clicks the smiley in the middle of the
  game.
\end{itemize}

\begin{code}
         playOn :: Mines-> Event ()
         playOn m = do always (sb # photo smCoolImg)
                       if all (not.untouched) (elems m) then 
                                        do always gameWon 
                                           gameOver
                          else play m
\end{code}

\texttt{playOn} takes care of the smiley when we (de)flagged or
explored a field. It also checks wether we have won with the previous
move; this is the case if there are no untouched fields left (i.e. all
fields are either cleared or flagged). This works because we are only
allowed to drop as many flags as there are mines on the field. 

\subsubsection{Game Over}

\texttt{gameLost} and \texttt{gameWon} just open message windows to
tell you that you've lost or won. The smiley also takes appropriate
action, feelin very sick or grinning inanely.

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

The windows are modal, so only after you closed them, the
game is really over and can be started again.

\begin{code}
         gameOver :: Event ()
         gameOver = start 
                    +> (choose (leCl++ riCl) >> gameOver) 
                    +> (choose press >> gameOver)
\end{code}

The \texttt{play} event changes into \texttt{gameOver} once the game
is over. It only lets you restart the game, and just swallows any of
the button events (\texttt{leCl} etc). If we didn't react to the
button events like that, they would still be in the event queue, and
be reacted to once the game restarts--- not what you want really!

Anyway, now we have set up the logic to play the game we can spawn an
event handler which waits for the game to start by synchronising on
the \texttt{start} event. All that is left is to return the buttons so
the main function can pack them. (Note that we bound events to buttons
before packing them, this is entirely possible.)

\begin{code}
     spawnEvent start
     return buttons
\end{code}

This is the end of the \texttt{buttons} function. Everything else is
just plain haskell. Ok, you're right, there is some tiny bits
left. Nobody explained how the numbers show up when a non-mine field
is explored, right? Okay, we'll come to that now.

\subsection{Modelling the Playing Field}

The playing field is represented by two arrays: one contains just the
buttons, since these are not going to change, and one contains the
actual state of a field, which is going to change: 
\begin{xcode}
data State = Cleared Int 
           | Unexplored { flagged :: Bool,
                          mine    :: Bool }

type Mines   = Array (Int, Int) State
type Buttons = Array (Int, Int) Button  
\end{xcode}
Three utility functions, \texttt{untouched}, \texttt{mines} and
\texttt{flags}, check wether is a state is untouched (neither cleared
nor flagged), and count the number of mines or flags on a field; we do
not show them here.

Now, the simple part is leaving and taking flags.

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
        if f || (sum (map flags (elems m)) < 
                    sum (map mines (elems m)))
        then do b!xy # (if not f then photo flagImg 
                        else photo zeroImg)
                return (m // [(xy, s{flagged= not f})])
        else return m 
\end{code}

If the the field is \emph{Cleared} which means it was explored earlier
the mines are left unchanged.  \texttt{Mines} are simply returned. If
the field is \texttt{Unexplored} and flagged we set it unflagged and
assign the \emph{zeroImg} to its \texttt{Button} and vice versa.
However, when dropping a flag we have make sure that there are not
more flags on the field than mines.

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

The function takes the same arguments but it returns a \texttt{Maybe}
of mines. If we try to explore a \texttt{Cleared} field, nothing
changes. If we try to explore an \texttt{Unexplored} field which is
flagged, we return the given argument and still nothing changes,
because it is inconvenient to accidently click a flag and get killed.
If we try to open an \texttt{Unexplored} field which is a mine the
function returns \texttt{Nothing}, and we have lost. And at last, when
every other case is weeded out, we \texttt{peek} inside an
\texttt{Unexplored}, nonflagged nomine field.

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
should be exactly one coordinate, namely the one we try to explore
right now. \texttt{adjMines} calculates the number of
mines around the field we explore; \texttt{adjacents} is a utility
function which takes a positition and returns the list of its adjacent
positions. 

We assign the corresponding image to the button's photo and change its
relief to flat (note how we compose configurations with
\texttt{(>>=)}). If the number of mines in the adjacent fields is 0 we
can savely explore all of these. So \emph{rest}, the list of
coordinates still to explore is united with the coordinates of the
adjecent fields which are still untouched. If we forgot to apply this
filter the exploration would go on forever exploring the same fields
over and over again, but this way the program explores all safe
fields, creating empty areas surrouned by numbers on the playfield
(see Fig.~\ref{fig:hsMines-one-click}, left).

\begin{figure}[htbp]
\begin{center}
\includegraphics[width=5cm]{img/hsMines/screenshot04}
~~~~~
\includegraphics[width=5cm]{img/hsMines/screenshot05}
\caption{hsMines after one lucky click, and a few lucky clicks later.}
\label{fig:hsMines-one-click}
\end{center}
\end{figure}

And just in case you don't believe this for real I finished the game
;-) (Fig.~\ref{fig:hsMines-one-click}, right).

The excurse through the \texttt{hsMInes} source is over for now. There
is still a couple of interessting functions (such as
\texttt{createMines}) left one can explore on it's own. Have fun.


%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "intro"
%%% End: 

