{- A DisplayType is a particular way of graphing objects.
   See the file RECURSION.
   -}
module DisplayTypes(
   DisplayType(..), -- The class of possible DisplayTypes
   WrappedDisplayType(..), -- existential type containing something that
      -- satisfies it.
      -- Instance of HasCodedValue,Eq

   -- Functions for extracting graph parameters.
   graphParms, -- :: HasGraphConfigs graphParms 
      -- => View -> WrappedDisplayType -> Source String -> IO graphParms

   -- NB.  Node and arc parameters are supplied by the particular
   -- object type instances.

   registerDisplayType, 
      -- :: DisplayType displayType => displayType -> IO ()
      -- Function which registers a particular Haskell type 
      -- displayType, so you can save and restore it using HasCodedValue.
      -- NB - this must only be done once for the Haskell type.
      -- It should not be done each type a new _value_ of type displayType
      -- is created.

   ShortDisplayType(..),
      -- This should be used to encode display types by tag.    

   importDisplayTypes, -- :: CodedValue -> View -> IO ()
      -- Decode the display types in this codedValue and store them.
   exportDisplayTypes, -- :: View -> IO CodedValue
      -- Encode the display types in a view.

   getAllDisplayTypeTypes, -- :: IO [WrappedDisplayType]
      -- Get all registered type-types for displays
   getAllDisplayTypes, -- :: View -> IO [WrappedDisplayType]
      -- Get all display types currently registered in this view.
   ) where


import qualified IOExts(unsafePerformIO)

import Registry
import Computation
import Dynamics
import Source

import GraphDisp
import GraphConfigure

import ViewType
import CodedValue
import GlobalRegistry
import {-# SOURCE #-} qualified DisplayView

class HasCodedValue displayType => DisplayType displayType where
   displayTypeTypeIdPrim :: displayType -> String
   -- This function should not look at its argument.
   -- To preserve uniqueness, the string should begin with the
   -- module name where the instance is defined.  If there is further 
   -- information, the module name should be followed by a period.  So
   -- for a module named "A", "A" and "A.B" are legal values for this
   -- string, but not "AB" or "C".

   displayTypeGlobalRegistry :: displayType -> GlobalRegistry displayType
   -- This returns the registry of all display types.
   displayTypeIdPrim :: displayType -> GlobalKey
   -- This returns the key for a displayType, used to access it in
   -- the global registry.

   graphParmsPrim :: HasGraphConfigs graphParms 
      => View -> displayType -> Source String -> IO graphParms
   -- The source will contain the current user title for this version.

   createDisplayTypeMenuItemPrim :: displayType -> Maybe (String,View -> IO ())
      -- This is a menu item (label + creation function) which creates a new
      -- display type and inserts it in the global registry.  We do not look
      -- at the argument.

      -- This is what the outside actually calls, but the implementation may
      -- instead choose to provide createObjectTypeMenuItemNoInsert.

   createDisplayTypeMenuItemNoInsert 
      :: Maybe (String,View -> IO (Maybe displayType))
      -- This is a menu item (label + creation function) which creates a new
      -- object type but does NOT insert it in the global registry.
      --  

   openDisplayMenuItemPrim :: 
      GraphAllConfig graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms
      => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
      -> displayType 
      -> Maybe (String,View 
         -> IO (Maybe (DisplayView.DisplayedView graph graphParms node 
            nodeType nodeTypeParms arc arcType arcTypeParms)))

   -- Default values

   createDisplayTypeMenuItemPrim badDisplayType =
      fmap
         (\ (label,createAct0) ->
            let
               createAct1 view =
                  do
                     displayTypeOpt <- createAct0 view
                     case displayTypeOpt of
                        Nothing -> done
                        Just (displayType :: displayType) ->
                           do
                              let 
                                 registry 
                                    = displayTypeGlobalRegistry displayType
                                 key = displayTypeIdPrim displayType
                              addToGlobalRegistry registry view key displayType
            in
               (label,createAct1)
            )
         createDisplayTypeMenuItemNoInsert


   createDisplayTypeMenuItemNoInsert = Nothing
   openDisplayMenuItemPrim _ _ = Nothing



-- ------------------------------------------------------------------
-- Wrapped display types
-- ------------------------------------------------------------------

data WrappedDisplayType = forall displayType .
   DisplayType displayType => WrappedDisplayType displayType

displayTypeTypeId :: WrappedDisplayType -> String
displayTypeTypeId (WrappedDisplayType displayType) =
   displayTypeTypeIdPrim displayType

graphParms :: HasGraphConfigs graphParms 
   => View -> WrappedDisplayType -> Source String -> IO graphParms
graphParms view (WrappedDisplayType displayType) source
   = graphParmsPrim view displayType source


-- ------------------------------------------------------------------
-- Comparing wrapped display types
-- ------------------------------------------------------------------

instance Eq WrappedDisplayType where
   (==) wd1 wd2 = (==) (displayTypeTypeId wd1) (displayTypeTypeId wd2)

-- ------------------------------------------------------------------
-- Registering particular displayTypeTypes
-- This is a parallel process to that for ObjectTypes.
-- ------------------------------------------------------------------

displayTypeDataRegistry :: Registry String WrappedDisplayType
displayTypeDataRegistry = IOExts.unsafePerformIO newRegistry
{-# NOINLINE displayTypeDataRegistry #-}

registerDisplayType :: DisplayType displayType => displayType -> IO ()
registerDisplayType displayType =
   do
      let
         typeTypeId = displayTypeTypeIdPrim displayType
      transformValue displayTypeDataRegistry typeTypeId
         (\ previous ->
            do
               case previous of
                  Nothing -> done
                  Just _ -> putStrLn 
                     ("Warning: for DisplayTypes.registerDisplayTypeType, "++
                        typeTypeId ++ " is multiply registered.")
               return (Just (WrappedDisplayType displayType),())
            )

-- ----------------------------------------------------------------
-- Accessing the GlobalRegistry's
-- ----------------------------------------------------------------

newtype ShortDisplayType displayType = ShortDisplayType displayType

-- Tycon for it
shortDisplayType_tyRep =  mkTyRep "DisplayTypes" "ShortDisplayType"

instance HasTyRep1 ShortDisplayType where
   tyRep1 _ = shortDisplayType_tyRep

instance DisplayType displayType => HasCodedValue (ShortDisplayType displayType) where
   encodeIO (ShortDisplayType displayType) codedValue view =
      do
         let 
            globalRegistry = displayTypeGlobalRegistry displayType
            key = displayTypeIdPrim displayType

         addToGlobalRegistry globalRegistry view key displayType
         encodeIO key codedValue view

   decodeIO codedValue0 view =
      do
         (key,codedValue1) <- safeDecodeIO codedValue0 view
         let 
            globalRegistry = displayTypeGlobalRegistry 
               (error "Don't look at me" :: displayType)
         displayType  <- lookupInGlobalRegistry globalRegistry view key
         return (ShortDisplayType displayType,codedValue1)
         
-- -----------------------------------------------------------------
-- Initialising and writing the Global Registries
-- -----------------------------------------------------------------

---
-- The String is the key into the displayTypeDataRegistry; 
type DisplayTypeData = [(String,CodedValue)]

---
-- Decode all the display type data for this value and put it in the
-- display type registers.
importDisplayTypes :: CodedValue -> View -> IO ()
importDisplayTypes codedValue view =
   do
      (displayTypeData :: DisplayTypeData) 
         <- doDecodeIO codedValue view
      sequence_ (
         map
            (\ (typeKey,codedValue) ->
               do
                  Just (WrappedDisplayType displayType) <-
                     getValueOpt displayTypeDataRegistry typeKey
                  importOneDisplayType displayType codedValue view
               )
            displayTypeData
         )

---
-- This decodes all the display types associated with a particular
-- Haskell type displayType, which is not looked at.  The codedValue represents
--  a list of type [displayType].
importOneDisplayType :: DisplayType displayType
   => displayType -> CodedValue -> View -> IO ()
importOneDisplayType displayType codedValue view =
   do
      let globalRegistry = displayTypeGlobalRegistry displayType
      addViewToGlobalRegistry globalRegistry view codedValue

---
-- Inverse to importDisplayTypes, producing a CodedValue for all types
-- present in this view.
exportDisplayTypes :: View -> IO CodedValue
exportDisplayTypes view =
-- We do however have to work slightly differently to importDisplayTypes,
-- going through the possible types rather than the coded value.
   do
      allDisplayTypes <- listRegistryContents displayTypeDataRegistry
      let
         processDisplayTypes [] acc = return acc
         processDisplayTypes 
            ((key,WrappedDisplayType displayType):rest) acc =
            do
               codedValueOpt <- exportOneDisplayType displayType view
               processDisplayTypes rest (
                  case codedValueOpt of
                  Nothing -> acc
                  Just codedValue -> (key,codedValue) : acc
                  )
      (displayTypeData :: DisplayTypeData) 
         <- processDisplayTypes allDisplayTypes []
      doEncodeIO displayTypeData view

---
-- This is the inverse to importOneDisplayType
exportOneDisplayType :: DisplayType displayType
   => displayType -> View -> IO (Maybe CodedValue)
exportOneDisplayType displayType view =
   do
      let globalRegistry = displayTypeGlobalRegistry displayType
      exportViewFromGlobalRegistry globalRegistry view

-- -----------------------------------------------------------------
-- We make WrappedDisplayType an instance of HasCodedValue.
-- The representation is as 
-- (displayTypeTypeIdPrim,ShortDisplayType displayType)
-- -----------------------------------------------------------------

wrappedDisplayType_tyRep = mkTyRep "DisplayTypes" "WrappedDisplayType"
instance HasTyRep WrappedDisplayType where
   tyRep _ = wrappedDisplayType_tyRep

instance HasCodedValue WrappedDisplayType where
   encodeIO (WrappedDisplayType displayType) codedValue0 view =
      do
         codedValue1 
            <- encodeIO (ShortDisplayType displayType) codedValue0 view
         codedValue2 
            <- encodeIO (displayTypeTypeIdPrim displayType) codedValue1 view
         return codedValue2

   decodeIO codedValue0 view =
      do
         (typeKey :: String,codedValue1) <- safeDecodeIO codedValue0 view
         Just (WrappedDisplayType displayType') <-
            getValueOpt displayTypeDataRegistry typeKey
         (displayType,codedValue2) <- decodeIO' displayType' codedValue1 view
         return (WrappedDisplayType displayType,codedValue2)

decodeIO' :: DisplayType displayType => displayType -> CodedValue -> View ->
   IO (displayType,CodedValue)
decodeIO' _ codedValue0 view = safeDecodeIO codedValue0 view

-- -----------------------------------------------------------------
-- Extract all display types
-- -----------------------------------------------------------------

getAllDisplayTypeTypes :: IO [WrappedDisplayType]
getAllDisplayTypeTypes =
   do
      contents <- listRegistryContents displayTypeDataRegistry
      return (map snd contents)

getAllDisplayTypes :: View -> IO [WrappedDisplayType]
getAllDisplayTypes view =
   do
      allDisplayTypeTypes <- getAllDisplayTypeTypes
      allDisplayTypesLists <- mapM
         (\ (WrappedDisplayType displayType) ->
            do
               let globalRegistry = displayTypeGlobalRegistry displayType
               displayTypes <- getAllElements globalRegistry view
               return (map WrappedDisplayType displayTypes)
            )
         allDisplayTypeTypes
      return (concat allDisplayTypesLists)