{- #########################################################################

MODULE        : NameSpace
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Global NameSpace, i.e. a name server shared between all
                threads running within a main program. 

TO BE DONE    : The current name space is flat. One may consider to structure
                it hierarchically using path names.


   ######################################################################### -}


module NameSpace (

        define,
        maybeRetrieve,
        retrieve,
        remove,
        exist,
        entryNotFound,
        typeMismatch,

        Dyn,
        TypeTag,
        Typeable(..)

        ) 
where

import qualified IOExts

import Maybes
import Variable
import Dynamics
import FiniteMap
import Computation
import Debug(debug)

-- --------------------------------------------------------------------------
-- Data Types
-- --------------------------------------------------------------------------

type NameSpace = PVar Env

type Env = FiniteMap String Dyn


-- --------------------------------------------------------------------------
--  Fetching NameSpace State
-- --------------------------------------------------------------------------

namespace :: NameSpace
namespace = IOExts.unsafePerformIO (newPVar emptyFM)

getNameSpaceEnv :: IO Env
getNameSpaceEnv = getVar namespace


-- --------------------------------------------------------------------------
-- NameSpace Commands
-- --------------------------------------------------------------------------

maybeRetrieve    :: Typeable a => String -> IO (Maybe a)
maybeRetrieve p =  do {
        env <- getNameSpaceEnv; 
        case lookupFM env p of
                Nothing -> return Nothing
                Just d -> return (fromDyn d)
        }

retrieve :: Typeable a => String -> IO a
retrieve p = do {
        env <- getNameSpaceEnv; 
        case lookupFM env p of
                Nothing -> raise entryNotFound
                Just d -> coerceIO d
        }


define :: Typeable a => String -> a -> IO ()
define p v = changeVar' namespace (\env -> (addToFM env p (toDyn v)))


remove :: String -> IO ()
remove p = changeVar' namespace (\env -> delFromFM (env::Env) (p::String))


exist :: String -> IO Bool
exist p = do { env <- getNameSpaceEnv; return (elemFM p env)}




entryNotFound = userError "Name space entry not found"




