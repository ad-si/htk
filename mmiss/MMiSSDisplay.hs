-- | This module describes the display type that is used for MMiSS graphs. 
module MMiSSDisplay(
   MMiSSDisplayType,
   registerMMiSSDisplay, -- :: IO ()
   ) where

import System.IO.Unsafe

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

import MMiSSDTDAssumptions
import MMiSSVariant

data MMiSSDisplayType = MMiSSDisplayType {
   name :: String,
   key :: GlobalKey,
   defaultVariantAttributes :: Attributes
   -- current settings of the variant attributes
   } deriving (Typeable)

-- -----------------------------------------------------------------
-- Instance of DisplayType
-- -----------------------------------------------------------------

instance HasBinary MMiSSDisplayType CodingMonad where
   writeBin = mapWrite (\ (MMiSSDisplayType {name = name,key = key,
      defaultVariantAttributes = defaultVariantAttributes}) 
         -> (name,key,defaultVariantAttributes))
   readBin = mapRead (\ (name,key,defaultVariantAttributes) ->
       MMiSSDisplayType {name = name,key = key,
          defaultVariantAttributes = defaultVariantAttributes})

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
                  defaultVariantAttributes = attributes
                  }))
-- -----------------------------------------------------------------
-- Registering the display
-- -----------------------------------------------------------------

registerMMiSSDisplay :: IO ()
registerMMiSSDisplay =
   registerDisplayType
      (error "Unknown MMiSSDisplay" :: MMiSSDisplayType)

-- -----------------------------------------------------------------
-- The Global Registry
-- -----------------------------------------------------------------

displayTypeRegistry :: GlobalRegistry MMiSSDisplayType
displayTypeRegistry = unsafePerformIO createGlobalRegistry
{-# NOINLINE displayTypeRegistry #-}
