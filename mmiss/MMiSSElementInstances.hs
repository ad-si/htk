-- | Instances of various classes for Element. 
module MMiSSElementInstances(
   ) where

import Text.XML.HaXml.Types

import Computation
import ExtendedPrelude
import AtomString
import Dynamics
import BinaryAll(Choice5(..))

import CodedValue

import MMiSSAttributes
import MMiSSDTD

-- ----------------------------------------------------------------------
-- Typeable
-- ----------------------------------------------------------------------

instance Typeable Element where
   typeOf _ = mkTypeRep "MMiSSElementInstances" "Element"

-- ----------------------------------------------------------------------
-- StringClass
-- ----------------------------------------------------------------------

instance StringClass Element where
   toString = toUglyExportableXml
   fromStringWE = xmlParseWE "Unknown source"

-- ------------------------------------------------------------------------
-- HasCodedValue instances
-- ------------------------------------------------------------------------

-- (1) Attributes

newtype XmlAttributes = XmlAttributes [Attribute] deriving (Typeable)

instance HasBinary XmlAttributes CodingMonad where
   writeBin = mapWriteViewIO 
      (\ view (XmlAttributes xmlAttributes) ->
         do         
            attributesWE <- fromXmlAttributes view xmlAttributes
            return (coerceWithError attributesWE)
         )
   readBin = mapReadViewIO
      (\ view attributes ->
         do
            xmlAttributes <- toXmlAttributes attributes
            return (XmlAttributes xmlAttributes)
         )

-- (2) Misc
instance Typeable Misc where
   typeOf _ = mkTypeRep "MMiSSElementInstances" "Misc"

instance Monad m => HasBinary Misc m where
   writeBin = mapWrite 
      (\ misc -> case misc of
         Comment comment -> Left comment
         PI pi -> Right pi
         )
   readBin = mapRead 
      (\ either -> case either of
         Left comment -> Comment comment
         Right pi -> PI pi
         )

-- (4) Reference
instance Typeable Reference where
   typeOf _ = mkTypeRep "MMiSSElementInstances" "Reference"

instance Monad m => HasBinary Reference m where
   writeBin = mapWrite (\ ref -> case ref of
      RefEntity n1 -> Left n1
      RefChar n2 -> Right n2
      )
   readBin = mapRead (\ either -> case either of
      Left n1 -> RefEntity n1
      Right n2 -> RefChar n2
      )

-- (4) Content
instance Typeable Content where
   typeOf _ = mkTypeRep "MMiSSElementInstances" "Content"


type PackedContent = Choice5 Element (Bool,CharData) Reference Misc ()

toPackedContent :: Content -> PackedContent
toPackedContent content = case content of
   CElem elem -> Choice1 elem
   CString b cd -> Choice2 (b,cd)
   CRef r -> Choice3 r
   CMisc m -> Choice4 m

fromPackedContent :: PackedContent -> Content
fromPackedContent choice = case choice of
   Choice1 elem -> CElem elem
   Choice2 (b,cd) -> CString b cd
   Choice3 r -> CRef r
   Choice4 m -> CMisc m

instance HasBinary Content CodingMonad where
   writeBin = mapWrite toPackedContent
   readBin = mapRead fromPackedContent

-- (5) Element

instance HasBinary Element CodingMonad where
   writeBin = mapWrite
      (\ (Elem name xmlAttributes contents) 
         -> (name,XmlAttributes xmlAttributes,contents))
   readBin = mapRead
      (\ (name,XmlAttributes xmlAttributes,contents) 
         -> Elem name xmlAttributes contents) 

-- ------------------------------------------------------------------------
-- Eq
-- ------------------------------------------------------------------------

mapElement :: Element -> (String,[Attribute],[Content])
mapElement (Elem n atts contents) = (n,atts,contents)

instance Eq Element where
   (==) = mapEq mapElement

{- AttValue and Reference aren't instanced as it turned out HaXml does this.

mapAttValue :: AttValue -> [Either String Reference]
mapAttValue (AttValue avs) = avs

instance Eq AttValue where
   (==) = mapEq mapAttValue

mapReference :: Reference -> Either EntityRef CharRef
mapReference (RefEntity e) = e
mapReference (RefChar c) = c

instance Eq Reference where
   (==) = mapEq mapReference
-}

instance Eq Content where
   (==) = mapEq toPackedContent

mapMisc :: Misc -> Either Comment ProcessingInstruction
mapMisc (Comment c) = Left c
mapMisc (PI pi) = Right pi

instance Eq Misc where
   (==) = mapEq mapMisc