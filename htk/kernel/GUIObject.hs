{- #########################################################################

MODULE        : GUIObject
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Internal structure of GUIObjects.


   ######################################################################### -}


module GUIObject (

        GUIOBJECT(..), 
        OST(..),
        FiniteMap,

        newGUIObject,
        newAbstractGUIObject,

        Object(..),
        ObjectID(..),
        getObjectNo,

        module GUIObjectKind,
        getObjectKind,
        setObjectKind,
        updObjectKind,

        module GUIObjectName,
        getObjectName,
        setObjectName,

        isPackedWidget,
        checkIfUnpacked,
        checkIfPacked,

        module GUIMethods,
        setMethods,

        makeChildObject,
        getChildObjects,
        getParentObjectID,
        hasParentObject,

        lookupConfigs, 
        deleteConfig,
        getConfigValue,
        setConfigValue,
        setPackConfig,
        getPackConfig,
        lookupConfig,

        objectAlreadyPacked,
        errorObjectNotPacked 
        ) where

import FiniteMap
import Concurrency
import Resources
import GUIObjectName
import GUIObjectKind
import GUIMethods
import Maybes
import Object
import Debug(debug)


-- --------------------------------------------------------------------------
-- GUI Object
-- --------------------------------------------------------------------------

data GUIOBJECT  = GUIOBJECT ObjectID (RVar OST)

data OST =                      -- GUI Object State
        OST {
                objectkind :: ObjectKind,       
                objectname :: (Maybe ObjectName),
                configopts :: (FiniteMap ConfigID GUIVALUE),
                packopts   :: (FiniteMap ConfigID GUIVALUE),
                childobjs  :: [GUIOBJECT],
                bindings   :: [Binding],
                parentobj  :: (Maybe ObjectID),
                methods    :: Methods
                }


-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Eq GUIOBJECT where
        (GUIOBJECT key1 _) == (GUIOBJECT key2 _) = key1 == key2
        wid1 /= wid2 = not (wid1 == wid2)

instance Ord GUIOBJECT where
        (GUIOBJECT key1 _) <= (GUIOBJECT key2 _) = key1 <= key2

instance Object GUIOBJECT where
        objectID (GUIOBJECT oid _) = oid


instance Synchronized GUIOBJECT where
        synchronize (GUIOBJECT _ rvar) = synchronize rvar


-- --------------------------------------------------------------------------
--  Object Creation
-- --------------------------------------------------------------------------

newGUIObject :: ObjectKind -> Methods -> IO GUIOBJECT
newGUIObject kind meths = do {
        oid <- newObject;
        mon <- newRVar (createObj kind meths);
        return (GUIOBJECT oid mon)
} where createObj t meths = 
                OST t Nothing emptyFM emptyFM [] [] Nothing meths


newAbstractGUIObject :: IO GUIOBJECT
newAbstractGUIObject = newGUIObject ABSTRACT voidMethods

  
-- --------------------------------------------------------------------------
--  GUI Object Identity
-- --------------------------------------------------------------------------

getObjectNo :: GUIOBJECT -> Int
getObjectNo (GUIOBJECT (ObjectID i) _) = i
{-# INLINE getObjectNo #-}


-- --------------------------------------------------------------------------
--  GUIObject Methods
-- --------------------------------------------------------------------------

setMethods :: GUIOBJECT -> Methods -> IO ()
setMethods (GUIOBJECT _ mon) m = changeVar' mon (\o -> o{methods = m})


-- --------------------------------------------------------------------------
--  Object Kind
-- --------------------------------------------------------------------------

getObjectKind :: GUIOBJECT -> IO ObjectKind
getObjectKind (GUIOBJECT _ mon) = withVar' mon objectkind


setObjectKind :: GUIOBJECT -> ObjectKind -> IO ()
setObjectKind (GUIOBJECT _ mon) k = changeVar' mon (\o -> o{objectkind = k})


updObjectKind :: GUIOBJECT -> (ObjectKind -> ObjectKind) -> IO ObjectKind
updObjectKind (GUIOBJECT _ mon) f = 
        updVar' mon (\o -> 
                let k' = f (objectkind o) in
                        (o{objectkind = k'},k')
                )


-- --------------------------------------------------------------------------
--  Pack Options
-- --------------------------------------------------------------------------

setPackConfig :: GUIValue a => GUIOBJECT -> ConfigID -> a -> IO ()
setPackConfig (wid @ (GUIOBJECT _ mon)) oid v  =
        changeVar' mon (\o -> o{packopts = (addToFM (packopts o) oid v')}) 
        where v' = toGUIValue v


getPackConfig :: GUIValue a => GUIOBJECT -> ConfigID -> IO a
getPackConfig (wid @ (GUIOBJECT _ mon)) cid =
        withVar mon (\o -> lookupConfig (packopts o) cid) 

        
-- --------------------------------------------------------------------------
--  Configuration Options
-- --------------------------------------------------------------------------

getConfigValue :: GUIOBJECT -> ConfigID -> IO (Maybe GUIVALUE)
getConfigValue (GUIOBJECT _ mon) cid =
        withVar' mon (\o -> lookupFM (configopts o) cid)

setConfigValue :: GUIOBJECT -> ConfigID -> GUIVALUE -> IO ()
setConfigValue (GUIOBJECT _ mon) cid val =
        changeVar' mon (\o -> o{configopts = addToFM (configopts o) cid val})


lookupConfigs :: GUIOBJECT -> IO [ConfigOption]
lookupConfigs (GUIOBJECT _ mon) = withVar' mon (\o -> fmToList (configopts o))


deleteConfig :: GUIOBJECT -> ConfigID -> IO ()
deleteConfig (wid @ (GUIOBJECT _ mon)) oid  =
        changeVar' mon (\o -> o{configopts = delFromFM (configopts o) oid})


lookupConfig :: GUIValue a => (FiniteMap ConfigID GUIVALUE) -> ConfigID -> IO a
lookupConfig opts cid =
        case (lookupFM opts cid) of
                Nothing -> return cdefault
                (Just v) -> fromGUIValueIO v


-- --------------------------------------------------------------------------
--  Object Name Related Functions 
-- --------------------------------------------------------------------------

isPackedWidget :: GUIOBJECT -> IO Bool
isPackedWidget (GUIOBJECT _ mon) = 
        withVar' mon objectname >>= return . maybeToBool


checkIfPacked :: GUIOBJECT -> IO ()
checkIfPacked wid = do {
        packed <- isPackedWidget wid;
        if packed then done else errorObjectNotPacked wid
}


checkIfUnpacked :: GUIOBJECT -> IO ()
checkIfUnpacked wid = do {
        packed <- isPackedWidget wid;
        if packed then raise objectAlreadyPacked else done
}


getObjectName :: GUIOBJECT -> IO (Maybe ObjectName)
getObjectName (GUIOBJECT _ mon) = withVar' mon objectname
        

setObjectName :: GUIOBJECT -> ObjectName -> IO ()
setObjectName (GUIOBJECT _ mon) name = 
        changeVar' mon (\o -> o{objectname = (Just name)})
        

-- --------------------------------------------------------------------------
--  Parent Child Relationship
-- --------------------------------------------------------------------------

makeChildObject :: GUIOBJECT -> GUIOBJECT -> IO ()
makeChildObject (pid @ (GUIOBJECT pkey par) ) (cid  @ (GUIOBJECT _ child)) = do{
        mpid <- getParentObjectID cid;
        case mpid of 
                (Just pid) -> done      {- can only be packed once -} 
                _ ->  do {
                        changeVar' par (newChild cid);
                        changeVar' child (\o -> o{parentobj = (Just pkey)})
                        }
}       where newChild cid o = o{childobjs = ((childobjs o) ++ [cid])}


getChildObjects :: GUIOBJECT -> IO [GUIOBJECT]
getChildObjects (GUIOBJECT _ mon) = withVar' mon childobjs
        

getParentObjectID :: GUIOBJECT -> IO (Maybe ObjectID)
getParentObjectID (GUIOBJECT _ mon) = withVar' mon parentobj


hasParentObject :: GUIOBJECT -> IO Bool
hasParentObject w = getParentObjectID w >>= return . maybeToBool


-- --------------------------------------------------------------------------
-- IOErrors
-- --------------------------------------------------------------------------

objectAlreadyPacked :: IOError
objectAlreadyPacked = userError "gui object is already packed"

errorObjectNotPacked :: GUIOBJECT -> IO a
errorObjectNotPacked (GUIOBJECT oid _) =
        raise (userError ("gui object is not packed: " ++ show oid))

