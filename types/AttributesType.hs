{- Encodes AttributesType.  The interface to this module is documented with
   HDoc.
   -}
module AttributesType(
   AttributesType,
   AttributesState(..),
   inputAttributes,
   updateAttributes,
   updateAttributesPrim,
   emptyAttributesType,
   isEmptyAttributesType,
   AttributeValue(..),
   needs,
   mkAttributesType,

   AttributeTypeKey,
   mkAttributeTypeKey,
   HasAttributeTypeKey(..),

   AttributeKey,
   mkAttributeKey,
   fromAttributeKey,

   registerAttribute,
   getAllAttributeTypeKeys,
   defaultAttributeTypeKey,
   registerAttributes,

   ExtraFormItem,
   mkExtraFormItem,
   readExtraFormItem,

   HasAttributesType(..),
   editObjectAttributes,   
   ) where

import Maybe

import IOExts
import ExtendedPrelude

import Dynamics
import Registry
import Computation
import Messages

import SimpleForm

import ViewType
import Link
import CodedValue 
import BasicObjects
import ObjectTypes


-- | AttributesState describes a possible return from an attributes setting
-- or updating function.
data AttributesState =
      NoForm    -- No attributes or extra form was specified, and no form was
                -- displayed
   |  Cancelled -- The user pressed Cancel, so no changes were made
   |  Changed   -- The user pressed Ok, so changes were (or at least may have
                -- been) made.
 
-- | An AttributesType gives conditions that a particular set of attributes
-- needs to satisfy (such as having an attribute with a given name and
-- a given type).  It also encodes the input method for that attribute.
-- 
-- AttributesType is an instance of HasCodedValue
data AttributesType = AttributesType [(AttributeKey,AttributeTypeKey)]
   deriving (Typeable)

instance Monad m => HasBinary AttributesType m where
   writeBin = mapWrite (\ (AttributesType list) -> list)
   readBin = mapRead (\ list -> AttributesType list)

-- | inputAttributes view attributesType extraFormItemOpt
-- constructs a new set of Attributes given the AttributesType and an
-- optional extra form item.  It returns Nothing if the user presses
-- Cancel.
-- 
-- If extraFormItemOpt is Nothing and the attributes type is empty we return
-- True.
inputAttributes :: View -> AttributesType -> Maybe (ExtraFormItem x) 
   -> IO (Maybe Attributes)
inputAttributes view attributesType extraFormItemOpt =
   do
      -- the values will go here.
      attributes <- newEmptyAttributes view
      success <- updateAttributesPrim "New Attributes" attributes 
         attributesType extraFormItemOpt
      case success of
         Cancelled -> return Nothing
         _ -> return (Just attributes)

-- | updateAttributes attributes attributesType extraFormItemOpt
-- puts up a form allowing the user to update the attributes
-- which should have type attributesType.
-- 
-- If extraFormItemOpt is Nothing and the attributes type is empty we don\'t
-- put any form up 
updateAttributes :: Attributes -> AttributesType -> Maybe (ExtraFormItem x) 
   -> IO AttributesState
updateAttributes attributes attributesType extraFormItemOpt = 
   updateAttributesPrim "Update Attributes" attributes attributesType 
      extraFormItemOpt

-- | updateAttributesPrim is like updateAttributes except it takes
-- as argument the String to be used as the title window, allowing
-- it also to be used for inputAttributes
updateAttributesPrim :: String -> Attributes -> AttributesType 
   -> Maybe (ExtraFormItem x) -> IO AttributesState
updateAttributesPrim title attributes (AttributesType attributesList) 
      extraFormItemOpt =
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

      -- Form of IO actions which set the attributes
      (forms1 :: [Form (IO ())]) <- mapM mkFormItem attributesList 

      let
         -- Form which constructs this, and the extraFormItem if present.
         forms = case extraFormItemOpt of
            Nothing -> forms1
            Just extraFormItem -> 
               (extraFormItemToForm extraFormItem) : forms1  

         -- combine two forms generating actions, generating
         -- one that does one action after the other.
         (///) :: Form (IO ()) -> Form (IO ()) -> Form (IO ())
         (///) form1 form2 = fmap (uncurry (>>)) (form1 // form2)

      case forms of 
         [] -> return NoForm
         _ ->
            do 
               let form = foldr1 (///) forms
               -- Now open the window. 
               actOpt <- doForm title form
               case actOpt of
                  Nothing -> return Cancelled
                  Just act ->
                     do
                        act -- set the attributes.
                        return Changed

-- | corresponds to an attributesType with no conditions on the attributes.
emptyAttributesType :: AttributesType
emptyAttributesType = AttributesType []

-- | returns True if the AttributesType has no attributes
isEmptyAttributesType :: AttributesType -> Bool
isEmptyAttributesType (AttributesType []) = True
isEmptyAttributesType _ = False

-- | Adds a new attribute
needs :: AttributeValue value => AttributeKey -> value -> AttributesType 
   -> AttributesType
needs key val (AttributesType list) =
   let
      typeKey = attributeTypeKey val 
   in
      AttributesType ((key,typeKey):list)

-- | Construct an AttributesType directly given a list of labels and
-- type keys.
mkAttributesType :: [(String,AttributeTypeKey)] -> AttributesType
mkAttributesType attributesList =
   let
      mkOne (keyStr,attributeTypeKey) = (mkAttributeKey keyStr,attributeTypeKey)
   in
      AttributesType (map mkOne attributesList)

-- -------------------------------------------------------------------
-- Support for additional form items.  These will go at the top,
-- if supplied.
-- -------------------------------------------------------------------

-- | An ExtraFormItem indicates an extra bit of data to be
-- read by inputAttributes.
data ExtraFormItem x = ExtraFormItem (Form x) (IORef (Maybe x))

-- | Constructs an ExtraFormItem from a form to be added on before
-- the attributes.
mkExtraFormItem :: Form x -> IO (ExtraFormItem x)
mkExtraFormItem form =
   do
      ioRef <- newIORef Nothing
      return (ExtraFormItem form ioRef)

-- | Read the value read from an extra form item by mkExtraFormItem.  
-- This must be used after inputAttributes, but before inputAttributes
-- is used again on the same ExtraFormItem.
readExtraFormItem :: ExtraFormItem x -> IO x
readExtraFormItem (ExtraFormItem _ ioRef) =
   do
      xOpt <- readIORef ioRef
      case xOpt of
         Nothing -> error 
            "AttributesType.readExtraFormItem used before inputAttributes"
         Just x -> return x

-- | Turns the ExtraFormItem into a form which returns an IO action which
-- make the value accessible for readExtraFormItem
extraFormItemToForm :: ExtraFormItem x -> Form (IO ())
extraFormItemToForm (ExtraFormItem form ioRef) =
   fmap
      (\ x -> writeIORef ioRef (Just x))
      form

-- -------------------------------------------------------------------
-- AttributeTypeKey and some instances.
-- -------------------------------------------------------------------

-- | AttributeTypeKey is needed to uniquely identify attribute types,
-- so that we can identify the attribute, given its key.  The keys
-- themselves must all be registered, in the same way as object type types
-- and display types are. 
newtype AttributeTypeKey = AttributeTypeKey String deriving (Ord,Eq,Typeable)

instance Monad m => HasBinary AttributeTypeKey m where
   writeBin = mapWrite (\ (AttributeTypeKey str) -> str)
   readBin = mapRead (\ str -> AttributeTypeKey str)

-- | The instance is for error messages.
instance Show AttributeTypeKey where
   show (AttributeTypeKey str) = last (splitByChar '.' str)

-- | Construct a new attribute key.  The arguments should be 
-- [module name] [type name], as for mkTyRep, to guarantee uniqueness.
mkAttributeTypeKey :: String -> String -> AttributeTypeKey
mkAttributeTypeKey modName tyName = AttributeTypeKey (modName++('.':tyName))

-- | the HasAttributeTypeKey contains those types which have an AttributeTypeKey
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

-- | attributeTypeKeyRegistry contains the current stock of registered 
-- attributes.  The result function takes the previous set of attributes,
-- if possible, and an AttributeKey and yields a form
-- which generates the action for updating the attributes to add a value
-- of this type.
attributeTypeKeyRegistry :: Registry AttributeTypeKey 
   (Attributes -> AttributeKey -> IO (Form (IO ())))
attributeTypeKeyRegistry = IOExts.unsafePerformIO newRegistry
{-# NOINLINE attributeTypeKeyRegistry #-}

-- | This must be done for every attribute value type at the start 
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
                        Nothing -> hasError ("Field "++
                           descriptor attributeKey++" isn't set")
                        Just x -> hasValue x
                        )  
                     valueOptForm
                  actForm = fmap
                     (\ value -> setValue attributes (name attributeKey) value)
                     valueForm
               return actForm   
      setValue attributeTypeKeyRegistry key mkForm 
        
-- | Get all AttributeTypeKey\'s + a suitable key for displaying them.
getAllAttributeTypeKeys :: IO [(AttributeTypeKey,String)]
getAllAttributeTypeKeys =
   do
      allKeys <- listKeys attributeTypeKeyRegistry
      let
         getPair (atk @ (AttributeTypeKey s)) = (atk,getLastPart s)
      return (map getPair allKeys)

-- | This is the attribute type which should be used if none other is
-- specified, corresponding to String, in the same format as 
-- getAllAttributeTypeKeys
-- Needless to say this key should have been registered.
defaultAttributeTypeKey :: (AttributeTypeKey,String)
defaultAttributeTypeKey =
   let
      atk@(AttributeTypeKey str) = attributeTypeKey ""
   in
      (atk,getLastPart str)

-- | Internal function which extracts the title from an attribute
-- string.
getLastPart :: String -> String
getLastPart [] = []
getLastPart ('.':rest) = 
   case getLastPart rest of
      [] -> rest
      s -> s
getLastPart (_:rest) = getLastPart rest

-- | registerAttributes registers all instances of AttributeValue declared
-- in this file, and should be done at the start of the program.
registerAttributes :: IO ()
registerAttributes =
   do
      let e = error "registerAttributes"
      registerAttribute (e :: String)
      registerAttribute (e :: Int)
      registerAttribute (e :: Integer)
      registerAttribute (e :: Bool)



-- -------------------------------------------------------------------
-- AttributeKey
-- -------------------------------------------------------------------

-- | An AttributeKey documents the key attached to a particular attribute
-- in an AttributeType.
data AttributeKey = AttributeKey {
   name :: String,
      -- this is the key used to identify the key as part of the
      -- attributes.  We should try to keep this unique . . .
   descriptor :: String
      -- this describes how to refer to the key on a form.
   } deriving (Typeable)

instance Monad m => HasBinary AttributeKey m where
   writeBin = mapWrite (\ attribute -> (name attribute,descriptor attribute))
   readBin = mapRead (\ (name,descriptor) -> AttributeKey {
      name = name,
      descriptor = descriptor
      })

-- | This makes an attribute key in the common case where we refer
-- to it on a form by its internal name. 
mkAttributeKey :: String -> AttributeKey
mkAttributeKey str = AttributeKey {
   name = str,
   descriptor = str
   }

-- | This returns the String by the the attribute key should be accessed.
fromAttributeKey :: AttributeKey -> String
fromAttributeKey attributeKey = name attributeKey
       
-- -------------------------------------------------------------------
-- The AttributeValue class and some instances.
-- -------------------------------------------------------------------

--- @doc AttributeValue
-- Class of values which can be attributes.  
class (HasCodedValue value,HasAttributeTypeKey value) 
   => AttributeValue value where

   -- | newFormEntry\' is identical to SimpleForm.newFormEntry except
   -- that it takes and returns (Maybe value).
   newFormEntry' :: FormLabel label => label -> Maybe value 
      -> Form (Maybe value)

-- | Instance 1: instances of FormTextFieldIO, where we simply take over
-- the Maybe instance defined in SimpleForm.  This includes strings
-- and numbers (provided they instance HasAttributeTypeKey).
instance (HasCodedValue value,HasAttributeTypeKey value,
      FormTextFieldIO value) => AttributeValue value where
   newFormEntry' = newFormEntry

-- | Instance 2: Bools
instance AttributeValue Bool where
   newFormEntry' lab boolOpt =
      let
         bool = fromMaybe False boolOpt
      in
         fmap Just (newFormEntry lab bool)
-- | Instance 3 - Radio Buttons.  
instance (HasConfigRadioButton value,Bounded value,Enum value,
      HasCodedValue value,HasAttributeTypeKey (Radio value)) 
      => AttributeValue (Radio value) where

   newFormEntry' lab valueOpt =
      let
         form0 = newFormEntry lab 
            (case valueOpt of 
               Nothing -> NoRadio
               Just value -> value
               )
      in
         fmap
            (\ radio -> case radio of
                  NoRadio -> Nothing
                  Radio _ -> Just radio
               )
            form0 

-- -------------------------------------------------------------------
-- The HasAttributesType class and the editObjectAttributes class.
-- -------------------------------------------------------------------

class HasAttributesType objectType where
   toAttributesType :: objectType -> AttributesType

editObjectAttributes :: (ObjectType objectType object,HasAttributes object,
   HasAttributesType objectType) 
   => View -> Link object -> IO ()
editObjectAttributes view link =
   do
      versioned <- fetchLink view link
      object <- readObject view versioned
      let 
         attributes = readPrimAttributes object
      success <- updateAttributes attributes (toAttributesType 
         (getObjectTypePrim object)) Nothing
      case success of
         Changed -> dirtyObject view versioned
         NoForm -> warningMess "No attributes defined for this object type"
         Cancelled -> done
