-- | Contains code enabling the user to define an AttributesType
-- NB.  At the moment this is disabled, and will always return the
-- empty attributes.  It should not be renabled again unless and until
-- bundles can handle attributes.
module Types.GetAttributesType(
   getAttributesType, -- :: IO AttributesType
   ) where

import Maybe

import Util.Computation

import Types.AttributesType

getAttributesType :: IO (Maybe AttributesType)
getAttributesType = return (Just emptyAttributesType)


{-
-- what we used to do
getAttributesType1 :: IO (Maybe AttributesType)
getAttributesType1 =
   do
      (attributeTypeKeys :: [(AttributeTypeKey,String)])
         <- getAllAttributeTypeKeys
      let
         -- Construct a map from strings to types (we will need this for
         -- unparsing the types as they come back from HTk)
         attributeTypeMap :: FiniteMap String AttributeTypeKey
         attributeTypeMap
            = listToFM (map (\ (key,s) -> (s,key)) attributeTypeKeys)

         attributeStringDefault :: String
         attributeStringDefault = snd defaultAttributeTypeKey

         attributeStringList1 :: [String]
         attributeStringList1 = map snd attributeTypeKeys

         -- Put the default string at the head of the list

         attributeStringList2 :: [String]
         attributeStringList2 = (attributeStringDefault :
            List.delete attributeStringDefault attributeStringList1)

         -- Pad the strings so that they all have the same length.  This
         -- prevents wish having to resize everything when we select a new
         -- type
         maxLen :: Int
         maxLen = foldl max 0 (map length attributeStringList2)

         attributeStringList3 :: [String]
         attributeStringList3 =
            map
               (\ s -> s++replicate (maxLen - length s) ' ')
               attributeStringList2

         -- Define the option menu for the type
         attributeTypeStringForm :: Form String
         attributeTypeStringForm = newFormOptionMenu attributeStringList3

         -- Convert it to a type
         attributeTypeForm :: Form AttributeTypeKey
         attributeTypeForm =
            fmap
               (\ answer ->
                  lookupWithDefaultFM
                     attributeTypeMap
                     (error
                        "GetAttributesType: wish returned unexpected string")
                     (trimTrailing answer)
                  )
               attributeTypeStringForm

         -- Form for the key string
         keyStringForm :: Form String
         keyStringForm = newFormEntry "Name" ""

         -- Form for the key string with spaces trimmed
         keyStringFormTrimmed :: Form String
         keyStringFormTrimmed = fmap trimSpaces keyStringForm

         -- Form line for a key string + type
         oneAttributeForm :: Form (String,AttributeTypeKey)
         oneAttributeForm = keyStringFormTrimmed \\ attributeTypeForm

         -- Form in which empty key strings go to Nothing
         oneAttributeChecked :: Form (Maybe (String,AttributeTypeKey))
         oneAttributeChecked =
            fmap
               (\ ktk@(keyString,typeKey) -> if keyString == "" then Nothing
                  else Just ktk)
               oneAttributeForm

         -- All the attributes we ask for in one go.
         allAttributesForm :: Form [Maybe(String,AttributeTypeKey)]
         allAttributesForm = column (replicate 10 oneAttributeChecked)

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
                  hasValue (list,if more then Just set else Nothing)
               cF set ((ktk@(key,_)):rest) more acc =
                  if elementOf key set
                     then
                        hasError ("Attribute "++show key++
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
-}



