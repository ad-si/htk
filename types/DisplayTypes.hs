module DisplayTypes(
   DisplayType(..), -- The class of possible DisplayTypes
   WrappedDisplayType(..), -- existential type containing something that
      -- satisfies it.
      -- Instance of HasCodedValue

   -- Functions for extracting graph parameters.
   graphParms, -- :: HasGraphConfigs graphParms 
      -- => WrappedDisplayType -> graphParms

   -- NB.  Node and arc parameters are supplied by the particular
   -- object type instances.

   registerDisplayType, 
      -- :: DisplayType displayType => displayType -> IO ()
      -- Function which registers a particular Haskell type 
      -- displayType, so you can save and restore it using HasCodedValue.
      -- NB - this must only be done once for the Haskell type.
      -- It should not be done each type a new _value_ of type displayType
      -- is created.
   ) where

import qualified IOExts(unsafePerformIO)

import AtomString
import Registry
import Computation
import Dynamics

import GraphConfigure

import View
import CodedValue

class HasCodedValue displayType => DisplayType displayType where
   displayTypeTypeIdPrim :: displayType -> String
   -- This function should not look at its argument.

   graphParmsPrim :: HasGraphConfigs graphParms => displayType -> graphParms


-- ------------------------------------------------------------------
-- Wrapped display types
-- ------------------------------------------------------------------

data WrappedDisplayType = forall displayType .
   DisplayType displayType => WrappedDisplayType displayType

displayTypeTypeId :: WrappedDisplayType -> String
displayTypeTypeId (WrappedDisplayType displayType) =
   displayTypeTypeIdPrim displayType

graphParms :: HasGraphConfigs graphParms => WrappedDisplayType -> graphParms
graphParms (WrappedDisplayType displayType) =
   graphParmsPrim displayType

-- ------------------------------------------------------------------
-- Registering particular displayTypeTypes
-- ------------------------------------------------------------------

type DisplayTypeData displayType = CodedValue -> View -> 
   IO (displayType,CodedValue)

data WrappedDisplayTypeData = forall displayType . 
   DisplayType displayType => 
      WrappedDisplayTypeData (DisplayTypeData displayType)

displayTypeDataRegistry :: Registry String WrappedDisplayTypeData
displayTypeDataRegistry = IOExts.unsafePerformIO newRegistry

registerDisplayType :: DisplayType displayType => displayType -> IO ()
registerDisplayType (_ :: displayType) =
   do
      let
         (displayTypeData :: DisplayTypeData displayType) = decodeIO 
         typeTypeId = displayTypeTypeIdPrim (undefined :: displayType)
      transformValue displayTypeDataRegistry typeTypeId
         (\ previous ->
            do
               case previous of
                  Nothing -> done
                  Just _ -> putStrLn 
                     ("Warning: for DisplayTypes.registerDisplayTypeType, "++
                        typeTypeId ++ " is multiply registered.")
               return (Just (WrappedDisplayTypeData displayTypeData),())
            )


wrappedDisplayType_tag = mkTyCon "DisplayTypes" "WrappedDisplayType"

instance HasTyCon WrappedDisplayType where
   tyCon _ = wrappedDisplayType_tag

instance HasCodedValue WrappedDisplayType where
   encodeIO (WrappedDisplayType displayType) codedValue0 view =
      do
         let typeTypeId = displayTypeTypeIdPrim displayType
         encode2IO typeTypeId displayType codedValue0 view
   decodeIO codedValue0 view =
      do
         (typeTypeId :: String,codedValue1) <- decodeIO codedValue0 view
         (decoderOpt :: Maybe WrappedDisplayTypeData) 
            <- getValueOpt displayTypeDataRegistry typeTypeId
         case decoderOpt of
           Just (WrappedDisplayTypeData decoder) ->
              do
                 (displayType,codedValue2) 
                    <- decoder codedValue1 view
                 return (WrappedDisplayType displayType,codedValue2)   
           Nothing -> error 
              ("DisplayTypes: displayTypeType "++typeTypeId++
              " not registered")

