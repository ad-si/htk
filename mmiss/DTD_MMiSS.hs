module DTD_MMiSS where

import Xml2Haskell


{-Type decls-}

data Atom = Atom Atom_Attrs Textfragment
	  deriving (Eq , Show)
data Atom_Attrs = Atom_Attrs
    { atomAtomid :: (Maybe String)
    } deriving (Eq , Show)
newtype Textfragment = Textfragment [Textfragment_] 		deriving (Eq , Show)
data Textfragment_ = Textfragment_Str String
		   | Textfragment_Include Include
		   | Textfragment_Reference Reference
		   deriving (Eq , Show)
data Include = Include
    { includeIncludedid :: String
    } deriving (Eq , Show)
data Reference = Reference Reference_Attrs Textfragment
	       deriving (Eq , Show)
data Reference_Attrs = Reference_Attrs
    { referenceReferenceid :: String
    } deriving (Eq , Show)
data Paragraph = Paragraph Paragraph_Attrs [Atom]
	       deriving (Eq , Show)
data Paragraph_Attrs = Paragraph_Attrs
    { paragraphParaid :: (Maybe String)
    , paragraphParatitle :: (Maybe String)
    } deriving (Eq , Show)
data Section = Section Section_Attrs [Section_]
	     deriving (Eq , Show)
data Section_Attrs = Section_Attrs
    { sectionSectionid :: (Maybe String)
    , sectionSectiontitle :: String
    } deriving (Eq , Show)
data Section_ = Section_Package Package
	      | Section_Section Section
	      | Section_Paragraph Paragraph
	      deriving (Eq , Show)
data Package = Package Package_Attrs [Section]
	     deriving (Eq , Show)
data Package_Attrs = Package_Attrs
    { packagePackageid :: (Maybe String)
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
	  { atomAtomid = possibleA fromAttrToStr "atomid" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "atomid" (atomAtomid v)
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
	  { includeIncludedid = definiteA fromAttrToStr "include" "includedid" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "includedid" (includeIncludedid v)
	]
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
	  { referenceReferenceid = definiteA fromAttrToStr "reference" "referenceid" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "referenceid" (referenceReferenceid v)
	]
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
	  { paragraphParaid = possibleA fromAttrToStr "paraid" as
	  , paragraphParatitle = possibleA fromAttrToStr "paratitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "paraid" (paragraphParaid v)
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
	  { sectionSectionid = possibleA fromAttrToStr "sectionid" as
	  , sectionSectiontitle = definiteA fromAttrToStr "section" "sectiontitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "sectionid" (sectionSectionid v)
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
	  { packagePackageid = possibleA fromAttrToStr "packageid" as
	  , packagePackagetitle = definiteA fromAttrToStr "package" "packagetitle" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "packageid" (packagePackageid v)
	, toAttrFrStr "packagetitle" (packagePackagetitle v)
	]


{-Done-}
