module DTD_MMiSS where

import Xml2Haskell


{-Type decls-}

data Atom = Atom Atom_Attrs Textfragment
	  deriving (Eq , Show)
data Atom_Attrs = Atom_Attrs
    { atomLabel :: (Maybe String)
    } deriving (Eq , Show)
newtype Textfragment = Textfragment [Textfragment_] 		deriving (Eq , Show)
data Textfragment_ = Textfragment_Str String
		   | Textfragment_Include Include
		   | Textfragment_Reference Reference
		   deriving (Eq , Show)
data Include = Include
    { includeIncluded :: String
    , includeStatus :: (Defaultable Include_Status)
    } deriving (Eq , Show)
data Include_Status = Include_Status_Present  | 
		      Include_Status_Absent
		    deriving (Eq , Show)
data Reference = Reference Reference_Attrs Textfragment
	       deriving (Eq , Show)
data Reference_Attrs = Reference_Attrs
    { referenceReferenced :: String
    , referenceStatus :: (Defaultable Reference_Status)
    } deriving (Eq , Show)
data Reference_Status = Reference_Status_Present  | 
			Reference_Status_Absent
		      deriving (Eq , Show)
data Paragraph = Paragraph Paragraph_Attrs [Atom]
	       deriving (Eq , Show)
data Paragraph_Attrs = Paragraph_Attrs
    { paragraphLabel :: (Maybe String)
    , paragraphParatitle :: (Maybe String)
    } deriving (Eq , Show)
data Section = Section Section_Attrs [Section_]
	     deriving (Eq , Show)
data Section_Attrs = Section_Attrs
    { sectionLabel :: (Maybe String)
    , sectionSectiontitle :: String
    } deriving (Eq , Show)
data Section_ = Section_Package Package
	      | Section_Section Section
	      | Section_Paragraph Paragraph
	      deriving (Eq , Show)
data Package = Package Package_Attrs [Section]
	     deriving (Eq , Show)
data Package_Attrs = Package_Attrs
    { packageLabel :: (Maybe String)
    , packagePackagetitle :: String
    } deriving (Eq , Show)


{-Instance decls-}

instance XmlContent Atom where
    fromElem (CElem (Elem "atom" as c0):rest) =
	(\(a,ca)->
	   (Just (Atom (fromAttrs as) a), rest))
	(definite fromElem "<textfragment>" "atom" c0)
    fromElem rest = (Nothing, rest)
    toElem (Atom as a) =
	[CElem (Elem "atom" (toAttrs as) (toElem a))]
instance XmlAttributes Atom_Attrs where
    fromAttrs as =
	Atom_Attrs
	  { atomLabel = possibleA fromAttrToStr "label" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (atomLabel v)
	]
instance XmlContent Textfragment where
    fromElem (CElem (Elem "textfragment" [] c0):rest) =
	(\(a,ca)->
	   (Just (Textfragment a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Textfragment a) =
	[CElem (Elem "textfragment" [] (concatMap toElem a))]
instance XmlContent Textfragment_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Textfragment_Str a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Textfragment_Include a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Textfragment_Reference a), rest)
			(Nothing,_) ->
			    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (Textfragment_Str a) = toText a
    toElem (Textfragment_Include a) = toElem a
    toElem (Textfragment_Reference a) = toElem a
instance XmlContent Include where
    fromElem (CElem (Elem "include" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "include" (toAttrs as) [])]
instance XmlAttributes Include where
    fromAttrs as =
	Include
	  { includeIncluded = definiteA fromAttrToStr "include" "included" as
	  , includeStatus = defaultA fromAttrToTyp Include_Status_Absent "status" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "included" (includeIncluded v)
	, defaultToAttr toAttrFrTyp "status" (includeStatus v)
	]
instance XmlAttrType Include_Status where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "present" = Just Include_Status_Present
	    translate "absent" = Just Include_Status_Absent
	    translate _ = Nothing
    toAttrFrTyp n Include_Status_Present = Just (n, str2attr "present")
    toAttrFrTyp n Include_Status_Absent = Just (n, str2attr "absent")
instance XmlContent Reference where
    fromElem (CElem (Elem "reference" as c0):rest) =
	(\(a,ca)->
	   (Just (Reference (fromAttrs as) a), rest))
	(definite fromElem "<textfragment>" "reference" c0)
    fromElem rest = (Nothing, rest)
    toElem (Reference as a) =
	[CElem (Elem "reference" (toAttrs as) (toElem a))]
instance XmlAttributes Reference_Attrs where
    fromAttrs as =
	Reference_Attrs
	  { referenceReferenced = definiteA fromAttrToStr "reference" "referenced" as
	  , referenceStatus = defaultA fromAttrToTyp Reference_Status_Absent "status" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "referenced" (referenceReferenced v)
	, defaultToAttr toAttrFrTyp "status" (referenceStatus v)
	]
instance XmlAttrType Reference_Status where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "present" = Just Reference_Status_Present
	    translate "absent" = Just Reference_Status_Absent
	    translate _ = Nothing
    toAttrFrTyp n Reference_Status_Present = Just (n, str2attr "present")
    toAttrFrTyp n Reference_Status_Absent = Just (n, str2attr "absent")
instance XmlContent Paragraph where
    fromElem (CElem (Elem "paragraph" as c0):rest) =
	(\(a,ca)->
	   (Just (Paragraph (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Paragraph as a) =
	[CElem (Elem "paragraph" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Paragraph_Attrs where
    fromAttrs as =
	Paragraph_Attrs
	  { paragraphLabel = possibleA fromAttrToStr "label" as
	  , paragraphParatitle = possibleA fromAttrToStr "paratitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (paragraphLabel v)
	, maybeToAttr toAttrFrStr "paratitle" (paragraphParatitle v)
	]
instance XmlContent Section where
    fromElem (CElem (Elem "section" as c0):rest) =
	(\(a,ca)->
	   (Just (Section (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Section as a) =
	[CElem (Elem "section" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Section_Attrs where
    fromAttrs as =
	Section_Attrs
	  { sectionLabel = possibleA fromAttrToStr "label" as
	  , sectionSectiontitle = definiteA fromAttrToStr "section" "sectiontitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (sectionLabel v)
	, toAttrFrStr "sectiontitle" (sectionSectiontitle v)
	]
instance XmlContent Section_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (Section_Package a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Section_Section a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Section_Paragraph a), rest)
			(Nothing,_) ->
			    (Nothing, c0)
    fromElem rest = (Nothing, rest)
    toElem (Section_Package a) = toElem a
    toElem (Section_Section a) = toElem a
    toElem (Section_Paragraph a) = toElem a
instance XmlContent Package where
    fromElem (CElem (Elem "package" as c0):rest) =
	(\(a,ca)->
	   (Just (Package (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem rest = (Nothing, rest)
    toElem (Package as a) =
	[CElem (Elem "package" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Package_Attrs where
    fromAttrs as =
	Package_Attrs
	  { packageLabel = possibleA fromAttrToStr "label" as
	  , packagePackagetitle = definiteA fromAttrToStr "package" "packagetitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (packageLabel v)
	, toAttrFrStr "packagetitle" (packagePackagetitle v)
	]


{-Done-}
