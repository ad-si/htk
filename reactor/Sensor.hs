{- #########################################################################

MODULE        : Sensor
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : State Polling Sensors etc. 


   ######################################################################### -}


module Sensor (
        HasReceiveEV(..),
        HasReceiveIO(..),
        Destructible(..),       

        Sensor,
        newSensor
        ) where

import Interaction
import Concurrency
import SIMClasses(Destructible(..))
import Debug(debug)

-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data Sensor a =  Sensor (Channel a) (Channel ())


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance HasReceiveEV Sensor a where
        receive (Sensor ch _) = receive ch

instance Destructible (Sensor a) where
        destroy (Sensor _ chc) = sendIO chc ()
        destroyed _ = inaction
                

-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newSensor :: Duration -> IO a -> IO (Sensor a)
newSensor time cmd = do {
        ch <- newChannel;
        chc <- newChannel;
        forkIO (sensor ch chc cmd);
        return (Sensor ch chc)
} where sensor ch chc cmd = do {
        st <- cmd;
        select [
                send ch st >>> do {delay time; sensor ch chc cmd},
                timeout time >>> sensor ch chc cmd,
                receive chc >>> deadlock
                ]
        }

