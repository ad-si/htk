-- | This file turns [XmlTypes.AttDef] lists into AttributesType's, and
-- [XmlTypes.Attribute] lists into Attribute's and back.  Various restrictions
-- are placed on these; for example defaults and non-String types are not
-- permitted. 
module MMiSSAttributes(
   fromXmlAttributesDef, -- :: [XmlTypes.AttDef] -> WithError AttributesType
   fromXmlAttributes, 
      -- :: View -> [XmlTypes.Attribute] -> IO (WithError Attributes)
   toXmlAttributes, -- :: Attributes -> IO [XmlTypes.Attribute]
   ) where

import Computation
import Registry

import Text.XML.HaXml.Types as XmlTypes

import BasicObjects
import View
import AttributesType

fromXmlAttributesDef :: [XmlTypes.AttDef] -> WithError AttributesType
fromXmlAttributesDef attDefs =
   -- Use Maybe instance of Monad.
   let
      fromXmlAttributeDef :: XmlTypes.AttDef -> WithError String
      fromXmlAttributeDef attDef =
         case attDef of
            AttDef name StringType IMPLIED -> hasValue name
            AttDef _ StringType _ 
               -> hasError "Attribute default is not IMPLIED"
            AttDef _ _ _ -> hasError "Attribute does not have String type"

      attributeStringsWE = concatWithError (map fromXmlAttributeDef attDefs)
   in
      mapWithError
         (\ attributeStrings ->
            foldl
               (\ attributesType string ->
                  (needs (mkAttributeKey string) "" attributesType)
                  )
               emptyAttributesType
               attributeStrings
            )
         attributeStringsWE

fromXmlAttributes :: View -> [XmlTypes.Attribute] -> IO (WithError Attributes)
fromXmlAttributes view xmlAttributes =
   do
      let
         (attributeSettingsWE :: WithError [(String,String)]) =
            concatWithError
               (map
                  (\ (name,AttValue attValue) ->
                     case attValue of
                        [Left sValue] -> hasValue (name,sValue)
                        [Right ref] -> hasError ("For attribute "++name++
                           ", value is a reference")
                        _ -> hasError ("For attribute "++name++
                           ", 0 or more than 1 setting.")
                     )
                  xmlAttributes
                  )
      attributes <- newEmptyAttributes view
      swapIOWithError (mapWithError
         (\ attributeSettings ->
            do
               mapM
                 (\ (name,value) -> setValue attributes name value)
                 attributeSettings
               return attributes
            )
         attributeSettingsWE
         )                     

toXmlAttributes :: Attributes -> IO [XmlTypes.Attribute]
toXmlAttributes attributes =
   do
      keys <- listKeys attributes
      mapM
         (\ key ->
            do
               value <- getValue attributes key
               return (key,AttValue [Left value])
            ) 
         keys      
