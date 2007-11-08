-- | This module allows the user to enter an arbitrary number of variant
-- attributes.
--
-- These are essentially just lists of key-value pairs of the form
-- [(String,Maybe String)] except that the strings may not begin or end
-- with a space, and duplicate keys or keys "Version" are not allowed.
module MMiSSGetVariantAttributes(
   getVariantAttributes, -- :: IO (Maybe [(String,Maybe String)])
   editVariantAttributes, -- :: [(String,Maybe String)]
      -- -> IO (Maybe [(String,Maybe String)])


   versionKey, -- :: String
   ) where

import Maybe
import qualified List

import DeprecatedSet

import Computation
import ExtendedPrelude

import SimpleForm

-- | Get a list of variant attributes.
getVariantAttributes :: IO (Maybe [(String,Maybe String)])
getVariantAttributes = editVariantAttributes []

-- | Edit the supplied list of variant attributes
editVariantAttributes :: [(String,Maybe String)]
   -> IO (Maybe [(String,Maybe String)])
editVariantAttributes = editVariantAttributes1 10


-- | Get a list of variant attributes, presenting the user with a window
-- where he can type up to n at a type.
editVariantAttributes1
   :: Int -> [(String,Maybe String)] -> IO (Maybe [(String,Maybe String)])
editVariantAttributes1 n oldVariants =
   editRemainingAttributes n emptySet oldVariants


editRemainingAttributes :: Int -> Set String -> [(String,Maybe String)]
   -> IO (Maybe [(String,Maybe String)])
editRemainingAttributes n keysSoFar remainingDefaults0 =
   do
      let
         trimSpaces1 :: String -> Maybe String
         trimSpaces1 s = case trimSpaces s of
            "" -> Nothing
            s -> Just s

         keyForm :: String -> Form (Maybe String)
         keyForm keyDefault = fmap trimSpaces1 (newFormEntry "key" keyDefault)

         valueForm :: Maybe String -> Form (Maybe String)
         valueForm valueDefault =
            fmap trimSpaces1 (newFormEntry "value" (fromMaybe "" valueDefault))

         keyValueForm :: (String,Maybe String)
            -> Form (Maybe (String,Maybe String))
         keyValueForm (keyDefault,valueDefault) =
            mapForm
               (\ (k,v) -> case (k,v) of
                  (Nothing,Nothing) -> return Nothing
                  (Nothing,Just _) -> fail
                     "Value given with no key"
                  (Just "Version",_) -> fail (
                     "You are not allowed to enter the \"Version\" attribute "
                     ++ "directly")
                  (Just key,Nothing) -> return (Just (key,Nothing))
                  (Just key,Just value) -> return (Just (key,Just value))
                  )
               (keyForm keyDefault SimpleForm.\\ valueForm valueDefault)

         remainingDefaults1 = List.filter
            (\ (key,value) -> not (elementOf key keysSoFar))
            remainingDefaults0

         (theseDefaults,remainingDefaults2) = splitAt n remainingDefaults1

         keyValueForms0 :: [Form (Maybe (String,Maybe String))]
         keyValueForms0 = fmap keyValueForm
            (take n (theseDefaults ++ repeat ("",Nothing)))

         keyValueForms1 :: Form [Maybe (String,Maybe String)]
         keyValueForms1 =
            foldr
               (\ form1 form2 ->
                  fmap (\ (kv,list) -> kv:list) (form1 // form2)
                  )
               (fmap (const []) emptyForm)
               keyValueForms0

         keyValueForms2 :: Form [(String,Maybe String)]
         keyValueForms2 = fmap catMaybes keyValueForms1

         keyValueForms3 :: Set String
            -> Form ([(String,Maybe String)],Set String)
         keyValueForms3 keysSoFar =
            let
               checkFn :: Set String -> [(String,Maybe String)]
                  -> WithError (Set String)
               checkFn keysSoFar [] = return keysSoFar
               checkFn keysSoFar ((key,_):otherValues) =
                  if elementOf key keysSoFar
                     then
                        fail ("Element " ++ key
                           ++ " is specified multiple times")
                     else
                        checkFn (addToSet keysSoFar key) otherValues
            in
               mapForm
                  (\ keyValues ->
                     do
                        set <- checkFn keysSoFar keyValues
                        return (keyValues,set)
                     )
                  keyValueForms2

         queryMore :: Form Bool
         queryMore = newFormEntry "Edit More Attributes?" False


      formResult <- doForm "Variant Attributes"
         ((keyValueForms3 keysSoFar) // queryMore)
      case formResult of
         Nothing -> return Nothing
         Just ((list,keysSoFar2),False) ->
            let
               remainingAttributes3 = List.filter
                  (\ (key,value) -> not (elementOf key keysSoFar2))
                  remainingDefaults2
            in
               return (Just (list ++ remainingAttributes3))
         Just ((list,keysSoFar2),True) ->
            do
               remainingAttributesOpt
                  <- editRemainingAttributes n keysSoFar2 remainingDefaults2
               return (fmap (list ++) remainingAttributesOpt)


-- | The name of the "Version" attribute, which the user is not allowed to
-- set
versionKey :: String
versionKey = "Version"
