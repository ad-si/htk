{- #########################################################################

MODULE        : Grep
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : grep encapsulation. 


   ######################################################################### -}


module Grep (
        Object(..),
        Tool(..),

        Grep,
        grep,
        receiveNextLine

        ) where

import WB
import Expect
import Debug(debug)



-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Grep = Grep Expect (MsgQueue (Maybe String))
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

grep :: String -> FilePath -> [Config PosixProcess] -> IO Grep
grep ptn fname confs = do {     
        exp <- newExpect "grep" (confs ++ [arguments [ptn,fname]]);
        mq <- newMsgQueue;
        dtool <- return (Grep exp mq);
        interactor (\iact -> 
                matchLine exp >>>= (sendIO mq) . Just 
           +>   destroyed dtool >>> do {sendIO mq Nothing; stop iact}
                );      
        return dtool;
} 


-- --------------------------------------------------------------------------
--  Get Next Grepped Line
-- --------------------------------------------------------------------------

receiveNextLine :: Grep -> EV (Maybe String)
receiveNextLine (Grep _ mq) = receive mq


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Grep where
        objectID (Grep tool _) = objectID tool

instance Tool Grep where
        getToolStatus (Grep tool _) = getToolStatus tool

instance UnixTool Grep where
        getUnixProcessID (Grep tool _) = getUnixProcessID tool

instance Destructible Grep where
        destroy (Grep tool mq) = do {sendIO mq Nothing; destroy tool}
        destroyed g@(Grep tool _) = destroyed tool



