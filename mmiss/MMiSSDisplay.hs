{- This module describes the display type that is used for MMiSS graphs. -}
module MMiSSDisplay(
   MMiSSDisplayType,
   ) where

import qualified IOExts(unsafePerformIO)

import Dynamics
import Computation

import GraphDisp
import GraphConfigure

import SimpleForm

import ViewType(getViewTitleSource)
import View
import CodedValue
import DisplayTypes
import BasicObjects
import GlobalRegistry
import AttributesType
import GlobalMenus
import DisplayView

import MMiSSVariant

data MMiSSDisplayType = MMiSSDisplayType {
   name :: String,
   key :: GlobalKey,
   variantAttributes :: Attributes
   -- current settings of the variant attributes
   }

-- -----------------------------------------------------------------
-- Instance of DisplayType
-- -----------------------------------------------------------------

mmissDisplayType_tyRep = mkTyRep "MMiSSDisplay" "MMiSSDisplayType"

instance HasTyRep MMiSSDisplayType where
   tyRep _ = mmissDisplayType_tyRep

instance HasCodedValue MMiSSDisplayType where
   encodeIO = mapEncodeIO (\ (MMiSSDisplayType {name = name,key = key,
      variantAttributes = variantAttributes}) -> (name,key,variantAttributes))
   decodeIO = mapDecodeIO (\ (name,key,variantAttributes) ->
       MMiSSDisplayType {name = name,key = key,
          variantAttributes = variantAttributes})

instance DisplayType MMiSSDisplayType where
   displayTypeTypeIdPrim _ = "MMiSSDisplay"

   displayTypeGlobalRegistry _ = displayTypeRegistry

   displayTypeIdPrim displayType = key displayType

   graphParmsPrim displaySort view displayType =
      do
         let
            graphTitleSource =
               fmap
                  (\ versionTitle -> GraphTitle (versionTitle++
                     ": "++name displayType++" listing")
                     )
                  (getViewTitleSource view)
 
         globalMenu <- newDefaultMenu displaySort view
         return (
            globalMenu $$
            AllowDragging True $$
            graphTitleSource $$
            emptyGraphParms
            )

   createDisplayTypeMenuItemNoInsert =
      Just ("New MMiSS Display",newMMiSSDisplay)

   openDisplayMenuItemPrim displaySort displayType =
      Just (name displayType,openGeneralDisplay displaySort displayType)
      
      
-- -----------------------------------------------------------------
-- Creating an MMiSSDisplay      
-- -----------------------------------------------------------------

newMMiSSDisplay :: View -> IO (Maybe MMiSSDisplayType)
newMMiSSDisplay view =
   do
      let
         nameForm = newFormEntry "Display Name" ""
      extraForm <- mkExtraFormItem nameForm
      attributesOpt 
         <- inputAttributes view variantAttributesType (Just extraForm)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               name <- readExtraFormItem extraForm
               key <- newKey displayTypeRegistry view
               return (Just (MMiSSDisplayType {
                  name = name,
                  key = key,
                  variantAttributes = attributes
                  }))
      
-- -----------------------------------------------------------------
-- The Global Registry
-- -----------------------------------------------------------------

displayTypeRegistry :: GlobalRegistry MMiSSDisplayType
displayTypeRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE displayTypeRegistry #-}
