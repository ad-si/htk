{- Encodes AttributesType.  The interface to this module is documented with
   HDoc.
   -}
module AttributesType(
   AttributesType,
   inputAttributes,
   updateAttributes,
   emptyAttributesType,
   AttributeValue(..),
   needs,

   AttributeTypeKey,
   mkAttributeTypeKey,
   HasAttributeTypeKey(..),

   registerAttribute,
   ) where

import Maybe

import qualified IOExts(unsafePerformIO)

import Dynamics
import Registry
import Computation

import SimpleForm

import ViewType
import CodedValue 
import BasicObjects

---
-- An AttributesType gives conditions that a particular set of attributes
-- needs to satisfy (such as having an attribute with a given name and
-- a given type).  It also encodes the input method for that attribute.
--
-- AttributesType is an instance of HasCodedValue
data AttributesType = AttributesType [(AttributeKey,AttributeTypeKey)]

attributesType_tyCon = mkTyCon "AttributesType" "AttributesType"
instance HasTyCon AttributesType where
   tyCon _ = attributesType_tyCon

instance HasCodedValue AttributesType where
   encodeIO = mapEncodeIO (\ (AttributesType list) -> list)
   decodeIO = mapDecodeIO (\ list -> AttributesType list)

---
-- inputAttributes attributesType
-- returns an Attributes value corresponding to this attributesType,
-- or Nothing if the user pressed Cancel.
inputAttributes :: View -> AttributesType -> IO (Maybe Attributes)
inputAttributes view attributesType =
   do
      -- the values will go here.
      attributes <- newEmptyAttributes view
      success <- 
         updateAttributesPrim "New Attributes" attributes attributesType
      return (if success then Just attributes else Nothing)

---
-- updateAttributes attributes attributesType
-- puts up a form allowing the user to update the attributes,
-- which should have type attributesType
updateAttributes :: Attributes -> AttributesType -> IO Bool
updateAttributes = updateAttributesPrim "Update Attributes"

---
-- updateAttributesPrim is like updateAttributes except it takes
-- as argument the String to be used as the title window, allowing
-- it also to be used for inputAttributes
updateAttributesPrim :: String -> Attributes -> AttributesType -> IO Bool
updateAttributesPrim title attributes (AttributesType attributesList) =
   do
      let
         -- Make a form item which generates a form returning the
         -- action to set the attribute.
         mkFormItem :: (AttributeKey,AttributeTypeKey) -> 
            IO (Form (IO ()))
         mkFormItem (key,typeKey) =
            do
               getForm <- getValue attributeTypeKeyRegistry typeKey
               getForm attributes key

         -- combine two forms generating actions, generating
         -- one that does one action after the other.
         (///) :: Form (IO ()) -> Form (IO ()) -> Form (IO ())
         (///) form1 form2 = fmap (uncurry (>>)) (form1 // form2)

      case attributesList of 
         [] -> return True
         _ ->
            do 
               (forms :: [Form (IO ())]) <- mapM mkFormItem attributesList
               let form = foldr1 (///) forms
               -- Now open the window. 
               actOpt <- doForm title form
               case actOpt of
                  Nothing -> return False
                  Just act ->
                     do
                        act -- set the attributes.
                        return True

---
-- corresponds to an attributesType with no conditions on the attributes.
emptyAttributesType :: AttributesType
emptyAttributesType = AttributesType []

---
-- Adds a new attribute
needs :: AttributeValue value => String -> value -> AttributesType 
   -> AttributesType
needs keyStr val (AttributesType list) =
   let
      key = mkAttributeKey keyStr
      typeKey = attributeTypeKey val 
   in
      AttributesType ((key,typeKey):list)

-- -------------------------------------------------------------------
-- AttributeTypeKey and some instances.
-- -------------------------------------------------------------------

---
-- AttributeTypeKey is needed to uniquely identify attribute types,
-- so that we can identify the attribute, given its key.  The keys
-- themselves must all be registered, in the same way as object type types
-- and display types are. 
newtype AttributeTypeKey = AttributeTypeKey String deriving (Ord,Eq)

attributeTypeKey_tyCon = mkTyCon "AttributesType" "AttributeTypeKey"
instance HasTyCon AttributeTypeKey where
   tyCon _ = attributeTypeKey_tyCon

instance HasCodedValue AttributeTypeKey where
   encodeIO = mapEncodeIO (\ (AttributeTypeKey str) -> str)
   decodeIO = mapDecodeIO (\ str -> AttributeTypeKey str)


---
-- Construct a new attribute key.  The arguments should be 
-- [module name] [type name], as for mkTyCon, to guarantee uniqueness.
mkAttributeTypeKey :: String -> String -> AttributeTypeKey
mkAttributeTypeKey modName tyName = AttributeTypeKey (modName++('.':tyName))

---
-- the HasAttributeTypeKey contains those types which have an AttributeTypeKey
class HasAttributeTypeKey value where
   attributeTypeKey :: value -> AttributeTypeKey

integer_attributeTypeKey = mkAttributeTypeKey "AttributesType" "Integer"
instance HasAttributeTypeKey Integer where
   attributeTypeKey _ = integer_attributeTypeKey

int_attributeTypeKey = mkAttributeTypeKey "AttributesType" "Int"
instance HasAttributeTypeKey Int where
   attributeTypeKey _ = int_attributeTypeKey

string_attributeTypeKey = mkAttributeTypeKey "AttributesType" "String"
instance HasAttributeTypeKey String where
   attributeTypeKey _ = string_attributeTypeKey

bool_attributeTypeKey = mkAttributeTypeKey "AttributesType" "Bool"
instance HasAttributeTypeKey Bool where
   attributeTypeKey _ = bool_attributeTypeKey


-- -------------------------------------------------------------------
-- The Register of AttributeTypes.
-- -------------------------------------------------------------------

---
-- attributeTypeKeyRegistry contains the current stock of registered 
-- attributes.  The result function takes the previous set of attributes,
-- if possible, and an AttributeKey and yields a form
-- which generates the action for updating the attributes to add a value
-- of this type.
attributeTypeKeyRegistry :: Registry AttributeTypeKey 
   (Attributes -> AttributeKey -> IO (Form (IO ())))
attributeTypeKeyRegistry = IOExts.unsafePerformIO newRegistry

---
-- This must be done for every attribute value type at the start 
-- of the program.  The value is not inspected.
registerAttribute :: AttributeValue value => value -> IO ()
registerAttribute (val :: value) =
   do
      let 
         key = attributeTypeKey val
      
         mkForm :: Attributes -> AttributeKey -> IO (Form (IO ()))
         mkForm attributes attributeKey =
            do
               (lastValOpt :: Maybe value) 
                  <- getValueOpt attributes (name attributeKey)
               let
                  valueOptForm = 
                     newFormEntry' (descriptor attributeKey) lastValOpt
                  valueForm = mapForm 
                     (\ valueOpt -> case valueOpt of
                        Nothing -> Left ("Field "++descriptor attributeKey++
                           " isn't set")
                        Just x -> Right x
                        )  
                     valueOptForm
                  actForm = fmap
                     (\ value -> setValue attributes (name attributeKey) value)
                     valueForm
               return actForm   
      setValue attributeTypeKeyRegistry key mkForm 
        
-- -------------------------------------------------------------------
-- AttributeKey
-- -------------------------------------------------------------------

---
-- An AttributeKey documents the key attached to a particular attribute
-- in an AttributeType.
data AttributeKey = AttributeKey {
   name :: String,
      -- this is the key used to identify the key as part of the
      -- attributes.  We should try to keep this unique . . .
   descriptor :: String
      -- this describes how to refer to the key on a form.
   }

attributeKey_tyCon = mkTyCon "AttributesType" "AttributeKey"
instance HasTyCon AttributeKey where
   tyCon _ = attributeKey_tyCon

instance HasCodedValue AttributeKey where
   encodeIO = mapEncodeIO 
      (\ attribute -> (name attribute,descriptor attribute))
   decodeIO = mapDecodeIO (\ (name,descriptor) -> AttributeKey {
      name = name,
      descriptor = descriptor
      })

---
-- This makes an attribute key in the common case where we refer
-- to it on a form by its internal name. 
mkAttributeKey :: String -> AttributeKey
mkAttributeKey str = AttributeKey {
   name = str,
   descriptor = str
   }
       
-- -------------------------------------------------------------------
-- The AttributeValue class and some instances.
-- -------------------------------------------------------------------

--- @doc AttributeValue
-- Class of values which can be attributes.  
class (HasCodedValue value,HasAttributeTypeKey value) 
      => AttributeValue value where
---
-- newFormEntry' is identical to SimpleForm.newFormEntry except
-- that it takes and returns (Maybe value).
   newFormEntry' :: FormLabel label => label -> Maybe value 
      -> Form (Maybe value)

---
-- Instance 1: instances of FormTextField, where we simply take over
-- the Maybe instance defined in SimpleForm.  This includes strings
-- and numbers (provided they instance HasAttributeTypeKey).
instance (HasCodedValue value,HasAttributeTypeKey value,FormTextField value) 
      => AttributeValue value where
   newFormEntry' = newFormEntry

---
-- Instance 2: Bools
instance AttributeValue Bool where
   newFormEntry' label boolOpt =
      let
         bool = fromMaybe False boolOpt
      in
         fmap Just (newFormEntry label bool)
         



