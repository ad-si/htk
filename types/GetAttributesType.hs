---
-- Contains code enabling the user to define an AttributesType
module GetAttributesType(
   getAttributesType, -- :: IO AttributesType
   ) where

import Maybe

import Set

import Computation
import ExtendedPrelude

import SimpleForm
import HTkMenu
import MenuType

import AttributesType

getAttributesType :: IO (Maybe AttributesType)
getAttributesType =
   do
      (attributeTypeKeys :: [(AttributeTypeKey,String)])
         <- getAllAttributeTypeKeys
      let
         -- Define a suitable menu
         attributeTypeMenu :: HTkMenu AttributeTypeKey
         attributeTypeMenu = 
            HTkMenu (Menu "Type" (map 
               (\ (typeKey,title) -> Button title typeKey) attributeTypeKeys))

         attributeTypeForm :: Form (Maybe AttributeTypeKey)
         attributeTypeForm = newFormMenu EmptyLabel attributeTypeMenu

         -- Form for the key string
         keyStringForm :: Form String
         keyStringForm = newFormEntry "Name" ""

         -- Form line for a key string + type
         oneAttributeForm :: Form (String,Maybe AttributeTypeKey)
         oneAttributeForm = keyStringForm \\ attributeTypeForm

         -- We check this ensuring that if the String is non-empty
         -- the attribute type key must be.  (We also trim spaces from
         -- the String here.)
         oneAttributeChecked :: Form (Maybe (String,AttributeTypeKey))
         oneAttributeChecked = 
            mapForm
               (\ (key,attributeOpt) ->
                  case (trimSpaces key,attributeOpt) of
                     ("",_) -> Right Nothing
                     (key,Just attributeTypeKey) 
                        -> Right (Just (key,attributeTypeKey))
                     (key,Nothing) 
                        -> Left ("No type specified for "++show key)
                  )
               oneAttributeForm
         
         -- All the attributes we ask for in one go.
         allAttributesForm :: Form [Maybe(String,AttributeTypeKey)]
         allAttributesForm = column [oneAttributeChecked | _ <- [1..10] ]

         -- List of all attributes actually set on this form
         condenseAttributesForm :: Form [(String,AttributeTypeKey)]
         condenseAttributesForm = fmap catMaybes allAttributesForm
         
         -- Query box asking whether more attributes are required
         queryMore :: Form Bool
         queryMore = newFormEntry "More Attributes" False

         -- Complete form to be shown, apart from checking function
         uncheckedForm :: Form ([(String,AttributeTypeKey)],Bool)
         uncheckedForm = condenseAttributesForm // queryMore

         -- Checking function which checks for duplicate keys
         -- (We only complain about the first one)
         checkForm :: Set String -> ([(String,AttributeTypeKey)],Bool)
            -> WithError ([(String,AttributeTypeKey)],Maybe (Set String))
         checkForm set (list,more) = cF set list more []
            where
               cF set [] more acc = 
                  Right (list,if more then Just set else Nothing)
               cF set ((ktk@(key,_)):rest) more acc =
                  if elementOf key set 
                     then 
                        Left ("Attribute "++show key++
                           " is multiply defined")
                     else
                        cF (addToSet set key) rest more (ktk:acc)

          -- Now here, finally, is the function which does the work.
          -- It returns Nothing if the operation was cancelled.
         getRemainingAttributes 
            :: Set String -> IO (Maybe [(String,AttributeTypeKey)])
         getRemainingAttributes set =
            do
               formResult <- doForm "Attributes" 
                  (mapForm (checkForm set) uncheckedForm)
               case formResult of
                  Nothing -> return Nothing
                  Just (list,Nothing) -> return (Just list)
                  Just (list,Just set2) ->
                     do
                        remainingAttributesOpt <- getRemainingAttributes set2
                        return (fmap (list ++) remainingAttributesOpt)
      -- Now get the attributes
      attributesOpt <- getRemainingAttributes emptySet
      case attributesOpt of
         Nothing -> return Nothing
         Just list ->  return (Just (mkAttributesType list))     
      



