{- #########################################################################

MODULE        : RadioGroup
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A radiogroup abstraction, almost similar to the one of Haggis:

                Finne, S. and Peyton Jones, S.: Composing Haggis,
                Proceedings of the Fifth Eurographics Workshop on
                Programming Paradigms for Computer Graphics, Springer, 1995.


   ######################################################################### -}


module RadioGroup (
        RadioGroup,
        radiogroup
        ) 
where

import Concurrency
import HTk
import RadioButton
import Interaction()
import Debug(debug)

-- --------------------------------------------------------------------------
-- RadioGroup 
-- --------------------------------------------------------------------------           
data RadioGroup a = RadioGroup [(RadioButton a)] (RVar (Maybe (RadioButton a)))


-- --------------------------------------------------------------------------
-- Creation 
-- --------------------------------------------------------------------------           
radiogroup :: [(RadioButton a)] -> IO (RadioGroup a)
radiogroup bts = do {
        try (foreach bts (selectionState Off));
        mv <- newRVar Nothing;
        return (RadioGroup bts mv)
        }

instance Synchronized (RadioGroup a) where
        synchronize (RadioGroup _ pv) = synchronize pv


-- --------------------------------------------------------------------------
-- Events/Triggers 
-- --------------------------------------------------------------------------           
instance HasTrigger RadioGroup a where
        getTrigger (RadioGroup bts cur) = return (choose (map handle bts))      
         where 
              handle bt = triggered bt >>>= \ v -> do {
                                sel <-getVar cur; 
                                setSel sel bt; 
                                return v
                                }
              setSel Nothing bt =  do
                        selectionState On bt
                        setVar cur (Just bt)
              setSel (Just bc) bt | (toGUIObject bc) == (toGUIObject bt) = 
                        done
              setSel (Just bc) bt = do
                        selectionState Off bc 
                        setSel Nothing bt

