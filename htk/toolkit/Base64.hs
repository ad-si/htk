en{- ------------------------------------------------------------------------
 -
 - Module Base64
 -
 - Author: ludi (just a few small changes to the grep encapsulation in
 -               uni/tools)
 - $Revision$ from $Date$
 -
 - ------------------------------------------------------------------------ -}


module Base64 (
        Object(..),
        Tool(..),

        Base64,
        base64,
        receiveNextLine

        ) where

import WB
import Expect
import Debug(debug)



-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Base64 = Base64 Expect (MsgQueue (Maybe String))

-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

base64 :: FilePath -> [Config PosixProcess] -> IO Base64
base64 fname confs = do {     
        exp <- newExpect "base64.pl" (confs ++ [arguments [fname]]);
        mq <- newMsgQueue;
        dtool <- return (Base64 exp mq);
        interactor (\iact -> 
                matchLine exp >>>= (sendIO mq) . Just 
           +>   destroyed dtool >>> do {sendIO mq Nothing; stop iact}
                );      
        return dtool;
}


-- --------------------------------------------------------------------------
--  Get Next Grepped Line
-- --------------------------------------------------------------------------

receiveNextLine :: Base64 -> EV (Maybe String)
receiveNextLine (Base64 _ mq) = receive mq


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Base64 where
        objectID (Base64 tool _) = objectID tool

instance Tool Base64 where
        getToolStatus (Base64 tool _) = getToolStatus tool

instance UnixTool Base64 where
        getUnixProcessID (Base64 tool _) = getUnixProcessID tool

instance Destructible Base64 where
        destroy (Base64 tool mq) = do {sendIO mq Nothing; destroy tool}
        destroyed g@(Base64 tool _) = destroyed tool


