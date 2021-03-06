-- | This module describes the display type that is used for MMiSS graphs.
module MMiSS.Display(
   MMiSSDisplayType,
   registerMMiSSDisplay, -- :: IO ()
   ) where

import System.IO.Unsafe

import Util.Dynamics
import Util.Computation

import Graphs.GraphDisp
import Graphs.GraphConfigure

import HTk.Toolkit.SimpleForm

import Types.ViewType(getViewTitleSource)
import Types.View
import Types.CodedValue
import Types.DisplayTypes
import Types.GlobalRegistry
import Types.GlobalMenus
import Types.DisplayView

data MMiSSDisplayType = MMiSSDisplayType {
   name :: String,
   key :: GlobalKey
   } deriving (Typeable)

-- -----------------------------------------------------------------
-- Instance of DisplayType
-- -----------------------------------------------------------------

instance HasBinary MMiSSDisplayType CodingMonad where
   writeBin = mapWrite (\ (MMiSSDisplayType {name = name,key = key})
         -> (name,key))
   readBin = mapRead (\ (name,key) ->
       MMiSSDisplayType {name = name,key = key})

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
      nameOpt <- doForm "Enter Display Name" nameForm
      case nameOpt of
         Nothing -> return Nothing
         Just name ->
            do
               key <- newKey displayTypeRegistry view
               return (Just (MMiSSDisplayType {
                  name = name,
                  key = key
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
