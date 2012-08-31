module Main where

import System.Random -- (randomRIO)
import Data.Array
import Data.List (union, nub)

import System.IO.Unsafe

import HTk.Toplevel.HTk hiding (State)
import HTk.Toolkit.DialogWin (createAlertWin,createMessageWin)

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
smWorriedImg = unsafePerformIO (newImage [imgData GIF "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" ])

{-# NOINLINE smWorriedImg #-}

smWinImg :: Image
smWinImg = unsafePerformIO (newImage [imgData GIF "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"])

{-# NOINLINE smWinImg #-}


main =
 do htk<- initHTk [withdrawMainWin]
    run htk normalSize

run :: HTk-> (Int, Int)-> IO ()
run htk currentSize =
 do main <- createToplevel [text "hsMines"]

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

    restartClick <- clicked restb
    quitClick <- clicked quitb

    csr1 <- clicked sr1
    csr2 <- clicked sr2
    csr3 <- clicked sr3
    csr4 <- clicked sr4
    csr5 <- clicked sr5

    sm <- newButton main [photo smSmileImg]
    startClick <- clicked sm

    pack sm [Side AtTop, PadY 20, PadX 20]

    restartCh <- newChannel

    bfr <- newFrame main [width (cm 10)]

    pack bfr [Side AtTop, PadX 15]

    size <- readTkVariable varSize
    allbuttons <- buttons bfr sm (receive restartCh) size
    delayWish $ mapM_ (\(xy, b)-> grid b
          [GridPos xy, GridPadX 1, GridPadY 1]) allbuttons

    let start :: IO ()
        start = do diff <- readTkVariable varDiff
                   sendIO restartCh diff

    -- start the menu handler
    stopmh<- spawnEvent (forever
          (startClick >>> start
        +> quitClick >>> destroy htk
        +> choose [csr1, csr2, csr3, csr4, csr5] >>>
             createMessageWin ("Changes come into effect\n"
               ++ "after \"Restart\".") []))

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

buttons :: Container par=> par-> Button-> Event Int
                           -> (Int, Int)
                           -> IO [((Int, Int), Button)]

buttons par sb startEv (size@(xmax, ymax)) =
  do buttons <- mapM (\xy->
       do b<- newButton par [photo starImg, relief Raised]
          return (xy, b)) [(x, y) | x <- [1.. xmax],
                                    y <- [1.. ymax]]

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


     let start :: Event ()
         start =
           startEv >>>= \d->
             do m <- createMines (snd (bounds bArr)) d
                sb # photo smSmileImg
                mapM_ (\b-> b # photo zeroImg >>=
                                relief Raised) (elems bArr)
                sync (play m)

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

         playOn :: Mines-> Event ()
         playOn m = do always (sb # photo smCoolImg)
                       if all (not.untouched) (elems m) then
                                        do always gameWon
                                           gameOver
                          else play m

         gameLost :: IO ()
         gameLost =
           do sb # photo smSadImg
              createAlertWin "*** BOOM!***\nYou lost." []
         gameWon :: IO ()
         gameWon =
           do sb # photo smWinImg
              createMessageWin "You have won!" []


         gameOver :: Event ()
         gameOver = start
                    +> (choose (leCl++ riCl) >> gameOver)
                    +> (choose press >> gameOver)

     spawnEvent start
     return buttons

-- drop or retrieve a flag (mouse right-click)

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

-- open up a field (mouse left-click)
-- returns Nothing, if we click on a hidden mine, the input if we
-- click on a flagged field (without a mine), and peeks at the field
-- otherwise

-- Crimson: I switched the order of Flag and Mine because it sucks to
-- accidently click a Flag and get killed...
-- I also put the Cleared _ expression on top because I think this saves
-- computation time.

open :: Buttons-> Mines-> (Int, Int)-> IO (Maybe Mines)
open b m xy =
  case m!xy of
    Cleared _                  -> return (Just m)
    Unexplored {flagged= True} -> return (Just m)
    Unexplored {mine= True}    -> return Nothing
    _ -> peek b m [xy] >>= return. Just

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
   in do (b!xy)# photo (getImg adjMines) >>= relief Flat
         if adjMines == 0 then
            peek b nu (rest `union`
                      (filter (untouched. (m !))
                              (adjacents m xy)))
            else peek b nu rest

