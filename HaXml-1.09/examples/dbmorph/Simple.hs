module Simple where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Article = Article (Maybe (Title,(Maybe Subtitle)))
		       (OneOf2 (List1 Abstract) [Section])
	     deriving (Eq,Show)
newtype Title = Title String 		deriving (Eq,Show)
newtype Subtitle = Subtitle String 		deriving (Eq,Show)
newtype Abstract = Abstract String 		deriving (Eq,Show)
data Section = Section (Maybe Title) [Para]
	     deriving (Eq,Show)
newtype Para = Para [Para_] 		deriving (Eq,Show)
data Para_ = Para_Str String
	   | Para_Computeroutput Computeroutput
	   deriving (Eq,Show)
newtype Computeroutput = Computeroutput String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Article where
    fromElem (CElem (Elem "article" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Article a b), rest))
	   (definite fromElem "OneOf" "article" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Article a b) =
	[CElem (Elem "article" [] (maybe [] toElem a ++
				   toElem b))]
instance XmlContent Title where
    fromElem (CElem (Elem "title" [] c0):rest) =
	(\(a,ca)->
	   (Just (Title a), rest))
	(definite fromText "text" "title" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Title a) =
	[CElem (Elem "title" [] (toText a))]
instance XmlContent Subtitle where
    fromElem (CElem (Elem "subtitle" [] c0):rest) =
	(\(a,ca)->
	   (Just (Subtitle a), rest))
	(definite fromText "text" "subtitle" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subtitle a) =
	[CElem (Elem "subtitle" [] (toText a))]
instance XmlContent Abstract where
    fromElem (CElem (Elem "abstract" [] c0):rest) =
	(\(a,ca)->
	   (Just (Abstract a), rest))
	(definite fromText "text" "abstract" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Abstract a) =
	[CElem (Elem "abstract" [] (toText a))]
instance XmlContent Section where
    fromElem (CElem (Elem "section" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Section a b), rest))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Section a b) =
	[CElem (Elem "section" [] (maybe [] toElem a ++
				   concatMap toElem b))]
instance XmlContent Para where
    fromElem (CElem (Elem "para" [] c0):rest) =
	(\(a,ca)->
	   (Just (Para a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Para a) =
	[CElem (Elem "para" [] (concatMap toElem a))]
instance XmlContent Para_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Para_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Para_Computeroutput a), rest)
		(_,_) ->
		    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Para_Str a) = toText a
    toElem (Para_Computeroutput a) = toElem a
instance XmlContent Computeroutput where
    fromElem (CElem (Elem "computeroutput" [] c0):rest) =
	(\(a,ca)->
	   (Just (Computeroutput a), rest))
	(definite fromText "text" "computeroutput" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Computeroutput a) =
	[CElem (Elem "computeroutput" [] (toText a))]


{-Done-}
