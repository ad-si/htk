module Queue where


data Queue a = MkQueue [a] [a]

emptyQ :: Queue a
emptyQ =  MkQueue [] []


isEmptyQ :: Queue a -> Bool
isEmptyQ (MkQueue [] []) = True
isEmptyQ _ = False


lengthQ :: Queue a -> Int
lengthQ (MkQueue fl rl) = length fl + length rl


insertQ :: Queue a -> a -> Queue a
insertQ (MkQueue fl rl) e = MkQueue (e:fl) rl


headQ :: Queue a -> a
headQ (MkQueue fl []) = (head (reverse fl))
headQ (MkQueue _ rl) = (head rl)


tailQ :: Queue a -> Queue a
tailQ (MkQueue fl [] ) = MkQueue [] tl where (x : tl) = reverse fl
tailQ (MkQueue fl rl ) = MkQueue fl (tail rl)


frontQ :: Queue a -> Maybe a
frontQ (MkQueue [] []) = Nothing
frontQ (MkQueue fl []) = Just (head (reverse fl))
frontQ (MkQueue _ rl) = Just (head rl)

removeQ :: Queue a -> Maybe (a, Queue a)
removeQ (MkQueue [] [] ) = error "removeQ: Queue is empty"
removeQ (MkQueue fl [] ) = Just (x, MkQueue [] tl) where (x : tl) = reverse fl
removeQ (MkQueue fl rl ) = Just (head rl, MkQueue fl (tail rl))


import Debug(debug)
