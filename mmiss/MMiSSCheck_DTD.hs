module MMiSSCheck_DTD where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

newtype Checklist = Checklist (List1 (Check)) 		deriving (Eq,Show)
data Check = Check Check_Attrs (Maybe Message) [(Mmissobject)]
	   deriving (Eq,Show)
data Check_Attrs = Check_Attrs
    { checkName :: String
    , checkSuccess :: Check_success
    } deriving (Eq,Show)
data Check_success = Check_success_Yes  |  Check_success_No
		   deriving (Eq,Show)
newtype Message = Message String 		deriving (Eq,Show)
data Mmissobject = Mmissobject
    { mmissobjectId :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Checklist where
    fromElem (CElem (Elem "checklist" [] c0):rest) =
	(\(a,ca)->
	   (Just (Checklist a), rest))
	(definite fromElem "(check)+" "checklist" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Checklist a) =
	[CElem (Elem "checklist" [] (toElem a))]
instance XmlContent Check where
    fromElem (CElem (Elem "check" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Check (fromAttrs as) a b), rest))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Check as a b) =
	[CElem (Elem "check" (toAttrs as) (maybe [] toElem a ++
					   concatMap toElem b))]
instance XmlAttributes Check_Attrs where
    fromAttrs as =
	Check_Attrs
	  { checkName = definiteA fromAttrToStr "check" "name" as
	  , checkSuccess = definiteA fromAttrToTyp "check" "success" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (checkName v)
	, toAttrFrTyp "success" (checkSuccess v)
	]
instance XmlAttrType Check_success where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "Yes" = Just Check_success_Yes
	    translate "No" = Just Check_success_No
	    translate _ = Nothing
    toAttrFrTyp n Check_success_Yes = Just (n, str2attr "Yes")
    toAttrFrTyp n Check_success_No = Just (n, str2attr "No")
instance XmlContent Message where
    fromElem (CElem (Elem "message" [] c0):rest) =
	(\(a,ca)->
	   (Just (Message a), rest))
	(definite fromText "text" "message" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Message a) =
	[CElem (Elem "message" [] (toText a))]
instance XmlContent Mmissobject where
    fromElem (CElem (Elem "mmissobject" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "mmissobject" (toAttrs as) [])]
instance XmlAttributes Mmissobject where
    fromAttrs as =
	Mmissobject
	  { mmissobjectId = definiteA fromAttrToStr "mmissobject" "id" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "id" (mmissobjectId v)
	]


{-Done-}
