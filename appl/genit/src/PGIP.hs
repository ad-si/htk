{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - Encapsulation of the PGIP types.
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 -}


module PGIP(  
  ProverCmd(..), -- commands which can be sent to the prover
  PrfStateId, -- abstract id for prover states
  ProverMsg(..), -- messages from the prover:
  ProverAns(..), --  1. answers to previous commands,
  ProverConf(..), -- 2. configurations messages (only GUIConf a/t mo)
  GUIConf(..),

  newCmdId, -- :: IO CmdId

  showCmd, -- :: ProverCmd -> String
  parseMsg -- :: String-> Maybe ProverMsg
) where

import MarkupText
import Computation (Answer)
import Xml2Haskell

import IOExts(readIORef, writeIORef, newIORef, unsafePerformIO)
import DTD_pgip

-- Commands sent to and from the prover
--
-- This is the XGit-internal abstraction over PGIP. 

-- Commands sent to the prover
data ProverCmd = GoalCmd String String
               | ProverCmd String
               | UndoCmd
               | CloseCmd PrfStateId -- i.e. save
               | AbortCmd 
               | LoadCmd String -- use_thy etc.
               | RestoreCmd PrfStateId
               | QedCmd String 

type PrfStateId = String

-- Responses orignated from the prover
data ProverMsg = ProverAns  (Answer ProverAns)
               | ProverConf ProverConf

-- Answers to commands
data ProverAns = PrfState  (Maybe PrfStateId) MarkupText
               | PrfTerm   String MarkupText -- the string is the type
               | PrfStatus MarkupText


-- Configuration messages (sent during initialisation)	       
data ProverConf= GUIConf GUIConf

-- GUI configuration
data GUIConf   = NewType   { name :: String,
                             icon :: String}
               | NewOpn    { src  :: [String],
	                     trg  :: String,
			     cmd  :: String,
			     name :: String }
	       | NewPrfOpn { src  :: [String],
			     cmd  :: String,
			     name :: String }

newtype CmdId= CmdId String deriving (Eq, Ord)

newCmdId :: IO CmdId
newCmdId = do c<- readIORef r
              writeIORef r (c+ 1)
              return (CmdId ("cmd"++ show c))
           where r = IOExts.unsafePerformIO (newIORef 0)

-- further code goes here
toPGIP :: ProverCmd -> Pgip
toPGIP cmd = Pgip (Pgip_Attrs {}) []

fromPGIP :: Pgip-> ProverAns
fromPGIP _ = PrfState Nothing (prose "Bollocks")


-- format command into PGIP-String
showCmd :: ProverCmd -> String
showCmd p = "hello mister prover sir"


-- parse a message from the prover
parseMsg :: String-> Maybe ProverMsg
parseMsg str = 
  error "Not implemented."
{-	  
  case readXml str of
    Nothing -> Nothing
    Just pgip -> error "Not implemented"
-}

-- recall Answer a = Either Exception a


