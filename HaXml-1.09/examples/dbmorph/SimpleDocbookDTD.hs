module SimpleDocbookDTD where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Title = Title Title_Attrs [Title_]
	   deriving (Eq,Show)
data Title_Attrs = Title_Attrs
    { titlePagenum :: (Maybe String)
    , titleId :: (Maybe String)
    , titleLang :: (Maybe String)
    , titleRevisionflag :: (Maybe Title_Revisionflag)
    , titleRole :: (Maybe String)
    } deriving (Eq,Show)
data Title_ = Title_Str String
	    | Title_Footnoteref Footnoteref
	    | Title_Xref Xref
	    | Title_Abbrev Abbrev
	    | Title_Acronym Acronym
	    | Title_Citetitle Citetitle
	    | Title_Emphasis Emphasis
	    | Title_Footnote Footnote
	    | Title_Phrase Phrase
	    | Title_Quote Quote
	    | Title_Trademark Trademark
	    | Title_Link Link
	    | Title_Ulink Ulink
	    | Title_Command Command
	    | Title_Computeroutput Computeroutput
	    | Title_Email Email
	    | Title_Filename Filename
	    | Title_Literal Literal
	    | Title_Option Option
	    | Title_Replaceable Replaceable
	    | Title_Systemitem Systemitem
	    | Title_Userinput Userinput
	    | Title_Author Author
	    | Title_Corpauthor Corpauthor
	    | Title_Othercredit Othercredit
	    | Title_Revhistory Revhistory
	    | Title_Inlinemediaobject Inlinemediaobject
	    deriving (Eq,Show)
data Title_Revisionflag = Title_Revisionflag_Changed
			   |  Title_Revisionflag_Added  | 
			  Title_Revisionflag_Deleted  |  Title_Revisionflag_Off
			deriving (Eq,Show)
data Titleabbrev = Titleabbrev Titleabbrev_Attrs
			       [Titleabbrev_]
		 deriving (Eq,Show)
data Titleabbrev_Attrs = Titleabbrev_Attrs
    { titleabbrevId :: (Maybe String)
    , titleabbrevLang :: (Maybe String)
    , titleabbrevRevisionflag :: (Maybe Titleabbrev_Revisionflag)
    , titleabbrevRole :: (Maybe String)
    } deriving (Eq,Show)
data Titleabbrev_ = Titleabbrev_Str String
		  | Titleabbrev_Footnoteref Footnoteref
		  | Titleabbrev_Xref Xref
		  | Titleabbrev_Abbrev Abbrev
		  | Titleabbrev_Acronym Acronym
		  | Titleabbrev_Citetitle Citetitle
		  | Titleabbrev_Emphasis Emphasis
		  | Titleabbrev_Footnote Footnote
		  | Titleabbrev_Phrase Phrase
		  | Titleabbrev_Quote Quote
		  | Titleabbrev_Trademark Trademark
		  | Titleabbrev_Link Link
		  | Titleabbrev_Ulink Ulink
		  | Titleabbrev_Command Command
		  | Titleabbrev_Computeroutput Computeroutput
		  | Titleabbrev_Email Email
		  | Titleabbrev_Filename Filename
		  | Titleabbrev_Literal Literal
		  | Titleabbrev_Option Option
		  | Titleabbrev_Replaceable Replaceable
		  | Titleabbrev_Systemitem Systemitem
		  | Titleabbrev_Userinput Userinput
		  | Titleabbrev_Author Author
		  | Titleabbrev_Corpauthor Corpauthor
		  | Titleabbrev_Othercredit Othercredit
		  | Titleabbrev_Revhistory Revhistory
		  | Titleabbrev_Inlinemediaobject Inlinemediaobject
		  deriving (Eq,Show)
data Titleabbrev_Revisionflag = Titleabbrev_Revisionflag_Changed
				 |  Titleabbrev_Revisionflag_Added  | 
				Titleabbrev_Revisionflag_Deleted  | 
				Titleabbrev_Revisionflag_Off
			      deriving (Eq,Show)
data Subtitle = Subtitle Subtitle_Attrs [Subtitle_]
	      deriving (Eq,Show)
data Subtitle_Attrs = Subtitle_Attrs
    { subtitleId :: (Maybe String)
    , subtitleLang :: (Maybe String)
    , subtitleRevisionflag :: (Maybe Subtitle_Revisionflag)
    , subtitleRole :: (Maybe String)
    } deriving (Eq,Show)
data Subtitle_ = Subtitle_Str String
	       | Subtitle_Footnoteref Footnoteref
	       | Subtitle_Xref Xref
	       | Subtitle_Abbrev Abbrev
	       | Subtitle_Acronym Acronym
	       | Subtitle_Citetitle Citetitle
	       | Subtitle_Emphasis Emphasis
	       | Subtitle_Footnote Footnote
	       | Subtitle_Phrase Phrase
	       | Subtitle_Quote Quote
	       | Subtitle_Trademark Trademark
	       | Subtitle_Link Link
	       | Subtitle_Ulink Ulink
	       | Subtitle_Command Command
	       | Subtitle_Computeroutput Computeroutput
	       | Subtitle_Email Email
	       | Subtitle_Filename Filename
	       | Subtitle_Literal Literal
	       | Subtitle_Option Option
	       | Subtitle_Replaceable Replaceable
	       | Subtitle_Systemitem Systemitem
	       | Subtitle_Userinput Userinput
	       | Subtitle_Author Author
	       | Subtitle_Corpauthor Corpauthor
	       | Subtitle_Othercredit Othercredit
	       | Subtitle_Revhistory Revhistory
	       | Subtitle_Inlinemediaobject Inlinemediaobject
	       deriving (Eq,Show)
data Subtitle_Revisionflag = Subtitle_Revisionflag_Changed
			      |  Subtitle_Revisionflag_Added  | 
			     Subtitle_Revisionflag_Deleted  | 
			     Subtitle_Revisionflag_Off
			   deriving (Eq,Show)
data Bibliomixed = Bibliomixed Bibliomixed_Attrs
			       [Bibliomixed_]
		 deriving (Eq,Show)
data Bibliomixed_Attrs = Bibliomixed_Attrs
    { bibliomixedId :: (Maybe String)
    , bibliomixedLang :: (Maybe String)
    , bibliomixedRevisionflag :: (Maybe Bibliomixed_Revisionflag)
    , bibliomixedRole :: (Maybe String)
    } deriving (Eq,Show)
data Bibliomixed_ = Bibliomixed_Str String
		  | Bibliomixed_Abbrev Abbrev
		  | Bibliomixed_Abstract Abstract
		  | Bibliomixed_Author Author
		  | Bibliomixed_Authorgroup Authorgroup
		  | Bibliomixed_Bibliomisc Bibliomisc
		  | Bibliomixed_Copyright Copyright
		  | Bibliomixed_Corpauthor Corpauthor
		  | Bibliomixed_Date Date
		  | Bibliomixed_Edition Edition
		  | Bibliomixed_Editor Editor
		  | Bibliomixed_Issuenum Issuenum
		  | Bibliomixed_Othercredit Othercredit
		  | Bibliomixed_Pubdate Pubdate
		  | Bibliomixed_Publishername Publishername
		  | Bibliomixed_Releaseinfo Releaseinfo
		  | Bibliomixed_Revhistory Revhistory
		  | Bibliomixed_Subtitle Subtitle
		  | Bibliomixed_Title Title
		  | Bibliomixed_Titleabbrev Titleabbrev
		  | Bibliomixed_Volumenum Volumenum
		  | Bibliomixed_Citetitle Citetitle
		  | Bibliomixed_Honorific Honorific
		  | Bibliomixed_Firstname Firstname
		  | Bibliomixed_Surname Surname
		  | Bibliomixed_Lineage Lineage
		  | Bibliomixed_Othername Othername
		  | Bibliomixed_Affiliation Affiliation
		  | Bibliomixed_Authorblurb Authorblurb
		  | Bibliomixed_Bibliomset Bibliomset
		  deriving (Eq,Show)
data Bibliomixed_Revisionflag = Bibliomixed_Revisionflag_Changed
				 |  Bibliomixed_Revisionflag_Added  | 
				Bibliomixed_Revisionflag_Deleted  | 
				Bibliomixed_Revisionflag_Off
			      deriving (Eq,Show)
data Articleinfo = Articleinfo Articleinfo_Attrs
			       (List1 (OneOf32 Mediaobject Legalnotice Subjectset Keywordset Abbrev Abstract Author Authorgroup Bibliomisc Copyright Corpauthor Date Edition Editor Issuenum Othercredit Pubdate Publishername Releaseinfo Revhistory Subtitle Title Titleabbrev Volumenum Citetitle Honorific Firstname Surname Lineage Othername Affiliation Authorblurb))
		 deriving (Eq,Show)
data Articleinfo_Attrs = Articleinfo_Attrs
    { articleinfoId :: (Maybe String)
    , articleinfoLang :: (Maybe String)
    , articleinfoRevisionflag :: (Maybe Articleinfo_Revisionflag)
    , articleinfoRole :: (Maybe String)
    } deriving (Eq,Show)
data Articleinfo_Revisionflag = Articleinfo_Revisionflag_Changed
				 |  Articleinfo_Revisionflag_Added  | 
				Articleinfo_Revisionflag_Deleted  | 
				Articleinfo_Revisionflag_Off
			      deriving (Eq,Show)
data Bibliomset = Bibliomset Bibliomset_Attrs
			     [Bibliomset_]
		deriving (Eq,Show)
data Bibliomset_Attrs = Bibliomset_Attrs
    { bibliomsetRelation :: (Maybe String)
    , bibliomsetId :: (Maybe String)
    , bibliomsetLang :: (Maybe String)
    , bibliomsetRevisionflag :: (Maybe Bibliomset_Revisionflag)
    , bibliomsetRole :: (Maybe String)
    } deriving (Eq,Show)
data Bibliomset_ = Bibliomset_Str String
		 | Bibliomset_Abbrev Abbrev
		 | Bibliomset_Abstract Abstract
		 | Bibliomset_Author Author
		 | Bibliomset_Authorgroup Authorgroup
		 | Bibliomset_Bibliomisc Bibliomisc
		 | Bibliomset_Copyright Copyright
		 | Bibliomset_Corpauthor Corpauthor
		 | Bibliomset_Date Date
		 | Bibliomset_Edition Edition
		 | Bibliomset_Editor Editor
		 | Bibliomset_Issuenum Issuenum
		 | Bibliomset_Othercredit Othercredit
		 | Bibliomset_Pubdate Pubdate
		 | Bibliomset_Publishername Publishername
		 | Bibliomset_Releaseinfo Releaseinfo
		 | Bibliomset_Revhistory Revhistory
		 | Bibliomset_Subtitle Subtitle
		 | Bibliomset_Title Title
		 | Bibliomset_Titleabbrev Titleabbrev
		 | Bibliomset_Volumenum Volumenum
		 | Bibliomset_Citetitle Citetitle
		 | Bibliomset_Honorific Honorific
		 | Bibliomset_Firstname Firstname
		 | Bibliomset_Surname Surname
		 | Bibliomset_Lineage Lineage
		 | Bibliomset_Othername Othername
		 | Bibliomset_Affiliation Affiliation
		 | Bibliomset_Authorblurb Authorblurb
		 | Bibliomset_Bibliomset Bibliomset
		 deriving (Eq,Show)
data Bibliomset_Revisionflag = Bibliomset_Revisionflag_Changed
			        |  Bibliomset_Revisionflag_Added  | 
			       Bibliomset_Revisionflag_Deleted  | 
			       Bibliomset_Revisionflag_Off
			     deriving (Eq,Show)
data Bibliomisc = Bibliomisc Bibliomisc_Attrs
			     [Bibliomisc_]
		deriving (Eq,Show)
data Bibliomisc_Attrs = Bibliomisc_Attrs
    { bibliomiscId :: (Maybe String)
    , bibliomiscLang :: (Maybe String)
    , bibliomiscRevisionflag :: (Maybe Bibliomisc_Revisionflag)
    , bibliomiscRole :: (Maybe String)
    } deriving (Eq,Show)
data Bibliomisc_ = Bibliomisc_Str String
		 | Bibliomisc_Footnoteref Footnoteref
		 | Bibliomisc_Xref Xref
		 | Bibliomisc_Abbrev Abbrev
		 | Bibliomisc_Acronym Acronym
		 | Bibliomisc_Citetitle Citetitle
		 | Bibliomisc_Emphasis Emphasis
		 | Bibliomisc_Footnote Footnote
		 | Bibliomisc_Phrase Phrase
		 | Bibliomisc_Quote Quote
		 | Bibliomisc_Trademark Trademark
		 | Bibliomisc_Link Link
		 | Bibliomisc_Ulink Ulink
		 | Bibliomisc_Command Command
		 | Bibliomisc_Computeroutput Computeroutput
		 | Bibliomisc_Email Email
		 | Bibliomisc_Filename Filename
		 | Bibliomisc_Literal Literal
		 | Bibliomisc_Option Option
		 | Bibliomisc_Replaceable Replaceable
		 | Bibliomisc_Systemitem Systemitem
		 | Bibliomisc_Userinput Userinput
		 | Bibliomisc_Inlinemediaobject Inlinemediaobject
		 deriving (Eq,Show)
data Bibliomisc_Revisionflag = Bibliomisc_Revisionflag_Changed
			        |  Bibliomisc_Revisionflag_Added  | 
			       Bibliomisc_Revisionflag_Deleted  | 
			       Bibliomisc_Revisionflag_Off
			     deriving (Eq,Show)
data Subjectset = Subjectset Subjectset_Attrs
			     (List1 Subject)
		deriving (Eq,Show)
data Subjectset_Attrs = Subjectset_Attrs
    { subjectsetScheme :: (Maybe String)
    , subjectsetId :: (Maybe String)
    , subjectsetLang :: (Maybe String)
    , subjectsetRevisionflag :: (Maybe Subjectset_Revisionflag)
    , subjectsetRole :: (Maybe String)
    } deriving (Eq,Show)
data Subjectset_Revisionflag = Subjectset_Revisionflag_Changed
			        |  Subjectset_Revisionflag_Added  | 
			       Subjectset_Revisionflag_Deleted  | 
			       Subjectset_Revisionflag_Off
			     deriving (Eq,Show)
data Subject = Subject Subject_Attrs
		       (List1 Subjectterm)
	     deriving (Eq,Show)
data Subject_Attrs = Subject_Attrs
    { subjectWeight :: (Maybe String)
    , subjectId :: (Maybe String)
    , subjectLang :: (Maybe String)
    , subjectRevisionflag :: (Maybe Subject_Revisionflag)
    , subjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Subject_Revisionflag = Subject_Revisionflag_Changed
			     |  Subject_Revisionflag_Added  | 
			    Subject_Revisionflag_Deleted  | 
			    Subject_Revisionflag_Off
			  deriving (Eq,Show)
data Subjectterm = Subjectterm Subjectterm_Attrs
			       String
		 deriving (Eq,Show)
data Subjectterm_Attrs = Subjectterm_Attrs
    { subjecttermId :: (Maybe String)
    , subjecttermLang :: (Maybe String)
    , subjecttermRevisionflag :: (Maybe Subjectterm_Revisionflag)
    , subjecttermRole :: (Maybe String)
    } deriving (Eq,Show)
data Subjectterm_Revisionflag = Subjectterm_Revisionflag_Changed
				 |  Subjectterm_Revisionflag_Added  | 
				Subjectterm_Revisionflag_Deleted  | 
				Subjectterm_Revisionflag_Off
			      deriving (Eq,Show)
data Keywordset = Keywordset Keywordset_Attrs
			     (List1 Keyword)
		deriving (Eq,Show)
data Keywordset_Attrs = Keywordset_Attrs
    { keywordsetId :: (Maybe String)
    , keywordsetLang :: (Maybe String)
    , keywordsetRevisionflag :: (Maybe Keywordset_Revisionflag)
    , keywordsetRole :: (Maybe String)
    } deriving (Eq,Show)
data Keywordset_Revisionflag = Keywordset_Revisionflag_Changed
			        |  Keywordset_Revisionflag_Added  | 
			       Keywordset_Revisionflag_Deleted  | 
			       Keywordset_Revisionflag_Off
			     deriving (Eq,Show)
data Keyword = Keyword Keyword_Attrs String
	     deriving (Eq,Show)
data Keyword_Attrs = Keyword_Attrs
    { keywordId :: (Maybe String)
    , keywordLang :: (Maybe String)
    , keywordRevisionflag :: (Maybe Keyword_Revisionflag)
    , keywordRole :: (Maybe String)
    } deriving (Eq,Show)
data Keyword_Revisionflag = Keyword_Revisionflag_Changed
			     |  Keyword_Revisionflag_Added  | 
			    Keyword_Revisionflag_Deleted  | 
			    Keyword_Revisionflag_Off
			  deriving (Eq,Show)
data Sidebar = Sidebar Sidebar_Attrs
		       (Maybe (Title,(Maybe Titleabbrev)))
		       (List1 (OneOf13 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table))
	     deriving (Eq,Show)
data Sidebar_Attrs = Sidebar_Attrs
    { sidebarId :: (Maybe String)
    , sidebarLang :: (Maybe String)
    , sidebarRevisionflag :: (Maybe Sidebar_Revisionflag)
    , sidebarRole :: (Maybe String)
    } deriving (Eq,Show)
data Sidebar_Revisionflag = Sidebar_Revisionflag_Changed
			     |  Sidebar_Revisionflag_Added  | 
			    Sidebar_Revisionflag_Deleted  | 
			    Sidebar_Revisionflag_Off
			  deriving (Eq,Show)
data Abstract = Abstract Abstract_Attrs (Maybe Title)
			 (List1 (Para))
	      deriving (Eq,Show)
data Abstract_Attrs = Abstract_Attrs
    { abstractId :: (Maybe String)
    , abstractLang :: (Maybe String)
    , abstractRevisionflag :: (Maybe Abstract_Revisionflag)
    , abstractRole :: (Maybe String)
    } deriving (Eq,Show)
data Abstract_Revisionflag = Abstract_Revisionflag_Changed
			      |  Abstract_Revisionflag_Added  | 
			     Abstract_Revisionflag_Deleted  | 
			     Abstract_Revisionflag_Off
			   deriving (Eq,Show)
data Authorblurb = Authorblurb Authorblurb_Attrs
			       (Maybe Title) (List1 (Para))
		 deriving (Eq,Show)
data Authorblurb_Attrs = Authorblurb_Attrs
    { authorblurbId :: (Maybe String)
    , authorblurbLang :: (Maybe String)
    , authorblurbRevisionflag :: (Maybe Authorblurb_Revisionflag)
    , authorblurbRole :: (Maybe String)
    } deriving (Eq,Show)
data Authorblurb_Revisionflag = Authorblurb_Revisionflag_Changed
				 |  Authorblurb_Revisionflag_Added  | 
				Authorblurb_Revisionflag_Deleted  | 
				Authorblurb_Revisionflag_Off
			      deriving (Eq,Show)
data Blockquote = Blockquote Blockquote_Attrs
			     (Maybe Title) (Maybe Attribution)
			     (List1 (OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph))
		deriving (Eq,Show)
data Blockquote_Attrs = Blockquote_Attrs
    { blockquoteId :: (Maybe String)
    , blockquoteLang :: (Maybe String)
    , blockquoteRevisionflag :: (Maybe Blockquote_Revisionflag)
    , blockquoteRole :: (Maybe String)
    } deriving (Eq,Show)
data Blockquote_Revisionflag = Blockquote_Revisionflag_Changed
			        |  Blockquote_Revisionflag_Added  | 
			       Blockquote_Revisionflag_Deleted  | 
			       Blockquote_Revisionflag_Off
			     deriving (Eq,Show)
data Attribution = Attribution Attribution_Attrs
			       [Attribution_]
		 deriving (Eq,Show)
data Attribution_Attrs = Attribution_Attrs
    { attributionId :: (Maybe String)
    , attributionLang :: (Maybe String)
    , attributionRevisionflag :: (Maybe Attribution_Revisionflag)
    , attributionRole :: (Maybe String)
    } deriving (Eq,Show)
data Attribution_ = Attribution_Str String
		  | Attribution_Footnoteref Footnoteref
		  | Attribution_Xref Xref
		  | Attribution_Abbrev Abbrev
		  | Attribution_Acronym Acronym
		  | Attribution_Citetitle Citetitle
		  | Attribution_Emphasis Emphasis
		  | Attribution_Footnote Footnote
		  | Attribution_Phrase Phrase
		  | Attribution_Quote Quote
		  | Attribution_Trademark Trademark
		  | Attribution_Link Link
		  | Attribution_Ulink Ulink
		  | Attribution_Command Command
		  | Attribution_Computeroutput Computeroutput
		  | Attribution_Email Email
		  | Attribution_Filename Filename
		  | Attribution_Literal Literal
		  | Attribution_Option Option
		  | Attribution_Replaceable Replaceable
		  | Attribution_Systemitem Systemitem
		  | Attribution_Userinput Userinput
		  | Attribution_Inlinemediaobject Inlinemediaobject
		  deriving (Eq,Show)
data Attribution_Revisionflag = Attribution_Revisionflag_Changed
				 |  Attribution_Revisionflag_Added  | 
				Attribution_Revisionflag_Deleted  | 
				Attribution_Revisionflag_Off
			      deriving (Eq,Show)
data Epigraph = Epigraph Epigraph_Attrs
			 (Maybe Attribution)
			 (List1 (OneOf2 Para Literallayout))
	      deriving (Eq,Show)
data Epigraph_Attrs = Epigraph_Attrs
    { epigraphId :: (Maybe String)
    , epigraphLang :: (Maybe String)
    , epigraphRevisionflag :: (Maybe Epigraph_Revisionflag)
    , epigraphRole :: (Maybe String)
    } deriving (Eq,Show)
data Epigraph_Revisionflag = Epigraph_Revisionflag_Changed
			      |  Epigraph_Revisionflag_Added  | 
			     Epigraph_Revisionflag_Deleted  | 
			     Epigraph_Revisionflag_Off
			   deriving (Eq,Show)
data Footnote = Footnote Footnote_Attrs
			 (List1 (OneOf9 Itemizedlist Orderedlist Variablelist Literallayout Programlisting Para Blockquote Mediaobject Informaltable))
	      deriving (Eq,Show)
data Footnote_Attrs = Footnote_Attrs
    { footnoteLabel :: (Maybe String)
    , footnoteId :: (Maybe String)
    , footnoteLang :: (Maybe String)
    , footnoteRevisionflag :: (Maybe Footnote_Revisionflag)
    , footnoteRole :: (Maybe String)
    } deriving (Eq,Show)
data Footnote_Revisionflag = Footnote_Revisionflag_Changed
			      |  Footnote_Revisionflag_Added  | 
			     Footnote_Revisionflag_Deleted  | 
			     Footnote_Revisionflag_Off
			   deriving (Eq,Show)
data Para = Para Para_Attrs [Para_]
	  deriving (Eq,Show)
data Para_Attrs = Para_Attrs
    { paraId :: (Maybe String)
    , paraLang :: (Maybe String)
    , paraRevisionflag :: (Maybe Para_Revisionflag)
    , paraRole :: (Maybe String)
    } deriving (Eq,Show)
data Para_ = Para_Str String
	   | Para_Footnoteref Footnoteref
	   | Para_Xref Xref
	   | Para_Abbrev Abbrev
	   | Para_Acronym Acronym
	   | Para_Citetitle Citetitle
	   | Para_Emphasis Emphasis
	   | Para_Footnote Footnote
	   | Para_Phrase Phrase
	   | Para_Quote Quote
	   | Para_Trademark Trademark
	   | Para_Link Link
	   | Para_Ulink Ulink
	   | Para_Command Command
	   | Para_Computeroutput Computeroutput
	   | Para_Email Email
	   | Para_Filename Filename
	   | Para_Literal Literal
	   | Para_Option Option
	   | Para_Replaceable Replaceable
	   | Para_Systemitem Systemitem
	   | Para_Userinput Userinput
	   | Para_Inlinemediaobject Inlinemediaobject
	   deriving (Eq,Show)
data Para_Revisionflag = Para_Revisionflag_Changed
			  |  Para_Revisionflag_Added  | 
			 Para_Revisionflag_Deleted  |  Para_Revisionflag_Off
		       deriving (Eq,Show)
data Note = Note Note_Attrs (Maybe Title)
		 (List1 (OneOf12 Itemizedlist Orderedlist Variablelist Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table))
	  deriving (Eq,Show)
data Note_Attrs = Note_Attrs
    { noteId :: (Maybe String)
    , noteLang :: (Maybe String)
    , noteRevisionflag :: (Maybe Note_Revisionflag)
    , noteRole :: (Maybe String)
    } deriving (Eq,Show)
data Note_Revisionflag = Note_Revisionflag_Changed
			  |  Note_Revisionflag_Added  | 
			 Note_Revisionflag_Deleted  |  Note_Revisionflag_Off
		       deriving (Eq,Show)
data Itemizedlist = Itemizedlist Itemizedlist_Attrs
				 (Maybe (Title,(Maybe Titleabbrev)))
				 [(OneOf10 Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Abstract Authorblurb Epigraph)]
				 (List1 Listitem)
		  deriving (Eq,Show)
data Itemizedlist_Attrs = Itemizedlist_Attrs
    { itemizedlistSpacing :: (Maybe Itemizedlist_Spacing)
    , itemizedlistMark :: (Maybe String)
    , itemizedlistId :: (Maybe String)
    , itemizedlistLang :: (Maybe String)
    , itemizedlistRevisionflag :: (Maybe Itemizedlist_Revisionflag)
    , itemizedlistRole :: (Maybe String)
    } deriving (Eq,Show)
data Itemizedlist_Spacing = Itemizedlist_Spacing_Normal
			     |  Itemizedlist_Spacing_Compact
			  deriving (Eq,Show)
data Itemizedlist_Revisionflag = Itemizedlist_Revisionflag_Changed
				  |  Itemizedlist_Revisionflag_Added  | 
				 Itemizedlist_Revisionflag_Deleted  | 
				 Itemizedlist_Revisionflag_Off
			       deriving (Eq,Show)
data Orderedlist = Orderedlist Orderedlist_Attrs
			       (Maybe (Title,(Maybe Titleabbrev)))
			       [(OneOf10 Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Abstract Authorblurb Epigraph)]
			       (List1 Listitem)
		 deriving (Eq,Show)
data Orderedlist_Attrs = Orderedlist_Attrs
    { orderedlistNumeration :: (Maybe Orderedlist_Numeration)
    , orderedlistInheritnum :: (Defaultable Orderedlist_Inheritnum)
    , orderedlistContinuation :: (Defaultable Orderedlist_Continuation)
    , orderedlistSpacing :: (Maybe Orderedlist_Spacing)
    , orderedlistId :: (Maybe String)
    , orderedlistLang :: (Maybe String)
    , orderedlistRevisionflag :: (Maybe Orderedlist_Revisionflag)
    , orderedlistRole :: (Maybe String)
    } deriving (Eq,Show)
data Orderedlist_Numeration = Orderedlist_Numeration_Arabic
			       |  Orderedlist_Numeration_Upperalpha  | 
			      Orderedlist_Numeration_Loweralpha  | 
			      Orderedlist_Numeration_Upperroman  | 
			      Orderedlist_Numeration_Lowerroman
			    deriving (Eq,Show)
data Orderedlist_Inheritnum = Orderedlist_Inheritnum_Inherit
			       |  Orderedlist_Inheritnum_Ignore
			    deriving (Eq,Show)
data Orderedlist_Continuation = Orderedlist_Continuation_Continues
				 |  Orderedlist_Continuation_Restarts
			      deriving (Eq,Show)
data Orderedlist_Spacing = Orderedlist_Spacing_Normal
			    |  Orderedlist_Spacing_Compact
			 deriving (Eq,Show)
data Orderedlist_Revisionflag = Orderedlist_Revisionflag_Changed
				 |  Orderedlist_Revisionflag_Added  | 
				Orderedlist_Revisionflag_Deleted  | 
				Orderedlist_Revisionflag_Off
			      deriving (Eq,Show)
data Listitem = Listitem Listitem_Attrs
			 (List1 (OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph))
	      deriving (Eq,Show)
data Listitem_Attrs = Listitem_Attrs
    { listitemOverride :: (Maybe String)
    , listitemId :: (Maybe String)
    , listitemLang :: (Maybe String)
    , listitemRevisionflag :: (Maybe Listitem_Revisionflag)
    , listitemRole :: (Maybe String)
    } deriving (Eq,Show)
data Listitem_Revisionflag = Listitem_Revisionflag_Changed
			      |  Listitem_Revisionflag_Added  | 
			     Listitem_Revisionflag_Deleted  | 
			     Listitem_Revisionflag_Off
			   deriving (Eq,Show)
data Variablelist = Variablelist Variablelist_Attrs
				 (Maybe (Title,(Maybe Titleabbrev)))
				 [(OneOf10 Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Abstract Authorblurb Epigraph)]
				 (List1 Varlistentry)
		  deriving (Eq,Show)
data Variablelist_Attrs = Variablelist_Attrs
    { variablelistTermlength :: (Maybe String)
    , variablelistId :: (Maybe String)
    , variablelistLang :: (Maybe String)
    , variablelistRevisionflag :: (Maybe Variablelist_Revisionflag)
    , variablelistRole :: (Maybe String)
    } deriving (Eq,Show)
data Variablelist_Revisionflag = Variablelist_Revisionflag_Changed
				  |  Variablelist_Revisionflag_Added  | 
				 Variablelist_Revisionflag_Deleted  | 
				 Variablelist_Revisionflag_Off
			       deriving (Eq,Show)
data Varlistentry = Varlistentry Varlistentry_Attrs
				 (List1 Term) Listitem
		  deriving (Eq,Show)
data Varlistentry_Attrs = Varlistentry_Attrs
    { varlistentryId :: (Maybe String)
    , varlistentryLang :: (Maybe String)
    , varlistentryRevisionflag :: (Maybe Varlistentry_Revisionflag)
    , varlistentryRole :: (Maybe String)
    } deriving (Eq,Show)
data Varlistentry_Revisionflag = Varlistentry_Revisionflag_Changed
				  |  Varlistentry_Revisionflag_Added  | 
				 Varlistentry_Revisionflag_Deleted  | 
				 Varlistentry_Revisionflag_Off
			       deriving (Eq,Show)
data Term = Term Term_Attrs [Term_]
	  deriving (Eq,Show)
data Term_Attrs = Term_Attrs
    { termId :: (Maybe String)
    , termLang :: (Maybe String)
    , termRevisionflag :: (Maybe Term_Revisionflag)
    , termRole :: (Maybe String)
    } deriving (Eq,Show)
data Term_ = Term_Str String
	   | Term_Footnoteref Footnoteref
	   | Term_Xref Xref
	   | Term_Abbrev Abbrev
	   | Term_Acronym Acronym
	   | Term_Citetitle Citetitle
	   | Term_Emphasis Emphasis
	   | Term_Footnote Footnote
	   | Term_Phrase Phrase
	   | Term_Quote Quote
	   | Term_Trademark Trademark
	   | Term_Link Link
	   | Term_Ulink Ulink
	   | Term_Command Command
	   | Term_Computeroutput Computeroutput
	   | Term_Email Email
	   | Term_Filename Filename
	   | Term_Literal Literal
	   | Term_Option Option
	   | Term_Replaceable Replaceable
	   | Term_Systemitem Systemitem
	   | Term_Userinput Userinput
	   | Term_Inlinemediaobject Inlinemediaobject
	   deriving (Eq,Show)
data Term_Revisionflag = Term_Revisionflag_Changed
			  |  Term_Revisionflag_Added  | 
			 Term_Revisionflag_Deleted  |  Term_Revisionflag_Off
		       deriving (Eq,Show)
data Example = Example Example_Attrs Title
		       (Maybe Titleabbrev)
		       (List1 (OneOf9 Itemizedlist Orderedlist Variablelist Literallayout Programlisting Para Blockquote Mediaobject Informaltable))
	     deriving (Eq,Show)
data Example_Attrs = Example_Attrs
    { exampleLabel :: (Maybe String)
    , exampleWidth :: (Maybe String)
    , exampleId :: (Maybe String)
    , exampleLang :: (Maybe String)
    , exampleRevisionflag :: (Maybe Example_Revisionflag)
    , exampleRole :: (Maybe String)
    } deriving (Eq,Show)
data Example_Revisionflag = Example_Revisionflag_Changed
			     |  Example_Revisionflag_Added  | 
			    Example_Revisionflag_Deleted  | 
			    Example_Revisionflag_Off
			  deriving (Eq,Show)
data Programlisting = Programlisting Programlisting_Attrs
				     [Programlisting_]
		    deriving (Eq,Show)
data Programlisting_Attrs = Programlisting_Attrs
    { programlistingWidth :: (Maybe String)
    , programlistingFormat :: (Defaultable Programlisting_Format)
    , programlistingLinenumbering :: (Maybe Programlisting_Linenumbering)
    , programlistingId :: (Maybe String)
    , programlistingLang :: (Maybe String)
    , programlistingRevisionflag :: (Maybe Programlisting_Revisionflag)
    , programlistingRole :: (Maybe String)
    } deriving (Eq,Show)
data Programlisting_ = Programlisting_Str String
		     | Programlisting_Footnoteref Footnoteref
		     | Programlisting_Xref Xref
		     | Programlisting_Abbrev Abbrev
		     | Programlisting_Acronym Acronym
		     | Programlisting_Citetitle Citetitle
		     | Programlisting_Emphasis Emphasis
		     | Programlisting_Footnote Footnote
		     | Programlisting_Phrase Phrase
		     | Programlisting_Quote Quote
		     | Programlisting_Trademark Trademark
		     | Programlisting_Link Link
		     | Programlisting_Ulink Ulink
		     | Programlisting_Command Command
		     | Programlisting_Computeroutput Computeroutput
		     | Programlisting_Email Email
		     | Programlisting_Filename Filename
		     | Programlisting_Literal Literal
		     | Programlisting_Option Option
		     | Programlisting_Replaceable Replaceable
		     | Programlisting_Systemitem Systemitem
		     | Programlisting_Userinput Userinput
		     | Programlisting_Inlinemediaobject Inlinemediaobject
		     | Programlisting_Lineannotation Lineannotation
		     deriving (Eq,Show)
data Programlisting_Format = Programlisting_Format_Linespecific
			   deriving (Eq,Show)
data Programlisting_Linenumbering = Programlisting_Linenumbering_Numbered
				     |  Programlisting_Linenumbering_Unnumbered
				  deriving (Eq,Show)
data Programlisting_Revisionflag = Programlisting_Revisionflag_Changed
				    |  Programlisting_Revisionflag_Added  | 
				   Programlisting_Revisionflag_Deleted  | 
				   Programlisting_Revisionflag_Off
				 deriving (Eq,Show)
data Literallayout = Literallayout Literallayout_Attrs
				   [Literallayout_]
		   deriving (Eq,Show)
data Literallayout_Attrs = Literallayout_Attrs
    { literallayoutWidth :: (Maybe String)
    , literallayoutFormat :: (Defaultable Literallayout_Format)
    , literallayoutLinenumbering :: (Maybe Literallayout_Linenumbering)
    , literallayoutClass :: (Defaultable Literallayout_Class)
    , literallayoutId :: (Maybe String)
    , literallayoutLang :: (Maybe String)
    , literallayoutRevisionflag :: (Maybe Literallayout_Revisionflag)
    , literallayoutRole :: (Maybe String)
    } deriving (Eq,Show)
data Literallayout_ = Literallayout_Str String
		    | Literallayout_Footnoteref Footnoteref
		    | Literallayout_Xref Xref
		    | Literallayout_Abbrev Abbrev
		    | Literallayout_Acronym Acronym
		    | Literallayout_Citetitle Citetitle
		    | Literallayout_Emphasis Emphasis
		    | Literallayout_Footnote Footnote
		    | Literallayout_Phrase Phrase
		    | Literallayout_Quote Quote
		    | Literallayout_Trademark Trademark
		    | Literallayout_Link Link
		    | Literallayout_Ulink Ulink
		    | Literallayout_Command Command
		    | Literallayout_Computeroutput Computeroutput
		    | Literallayout_Email Email
		    | Literallayout_Filename Filename
		    | Literallayout_Literal Literal
		    | Literallayout_Option Option
		    | Literallayout_Replaceable Replaceable
		    | Literallayout_Systemitem Systemitem
		    | Literallayout_Userinput Userinput
		    | Literallayout_Inlinemediaobject Inlinemediaobject
		    | Literallayout_Lineannotation Lineannotation
		    deriving (Eq,Show)
data Literallayout_Format = Literallayout_Format_Linespecific
			  deriving (Eq,Show)
data Literallayout_Linenumbering = Literallayout_Linenumbering_Numbered
				    |  Literallayout_Linenumbering_Unnumbered
				 deriving (Eq,Show)
data Literallayout_Class = Literallayout_Class_Monospaced
			    |  Literallayout_Class_Normal
			 deriving (Eq,Show)
data Literallayout_Revisionflag = Literallayout_Revisionflag_Changed
				   |  Literallayout_Revisionflag_Added  | 
				  Literallayout_Revisionflag_Deleted  | 
				  Literallayout_Revisionflag_Off
				deriving (Eq,Show)
data Figure = Figure Figure_Attrs Title
		     (Maybe Titleabbrev)
		     (List1 (OneOf7 Literallayout Programlisting Blockquote Mediaobject Informaltable Link Ulink))
	    deriving (Eq,Show)
data Figure_Attrs = Figure_Attrs
    { figureFloat :: (Defaultable String)
    , figurePgwide :: (Maybe String)
    , figureLabel :: (Maybe String)
    , figureId :: (Maybe String)
    , figureLang :: (Maybe String)
    , figureRevisionflag :: (Maybe Figure_Revisionflag)
    , figureRole :: (Maybe String)
    } deriving (Eq,Show)
data Figure_Revisionflag = Figure_Revisionflag_Changed
			    |  Figure_Revisionflag_Added  | 
			   Figure_Revisionflag_Deleted  | 
			   Figure_Revisionflag_Off
			 deriving (Eq,Show)
data Mediaobject = Mediaobject Mediaobject_Attrs
			       (Maybe Objectinfo)
			       (List1 (OneOf4 Videoobject Audioobject Imageobject Textobject))
			       (Maybe Caption)
		 deriving (Eq,Show)
data Mediaobject_Attrs = Mediaobject_Attrs
    { mediaobjectId :: (Maybe String)
    , mediaobjectLang :: (Maybe String)
    , mediaobjectRevisionflag :: (Maybe Mediaobject_Revisionflag)
    , mediaobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Mediaobject_Revisionflag = Mediaobject_Revisionflag_Changed
				 |  Mediaobject_Revisionflag_Added  | 
				Mediaobject_Revisionflag_Deleted  | 
				Mediaobject_Revisionflag_Off
			      deriving (Eq,Show)
data Inlinemediaobject = Inlinemediaobject Inlinemediaobject_Attrs
					   (Maybe Objectinfo)
					   (List1 (OneOf4 Videoobject Audioobject Imageobject Textobject))
		       deriving (Eq,Show)
data Inlinemediaobject_Attrs = Inlinemediaobject_Attrs
    { inlinemediaobjectId :: (Maybe String)
    , inlinemediaobjectLang :: (Maybe String)
    , inlinemediaobjectRevisionflag :: (Maybe Inlinemediaobject_Revisionflag)
    , inlinemediaobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Inlinemediaobject_Revisionflag = Inlinemediaobject_Revisionflag_Changed
				       |  Inlinemediaobject_Revisionflag_Added
				       |  Inlinemediaobject_Revisionflag_Deleted
				       |  Inlinemediaobject_Revisionflag_Off
				    deriving (Eq,Show)
data Videoobject = Videoobject Videoobject_Attrs
			       (Maybe Objectinfo) Videodata
		 deriving (Eq,Show)
data Videoobject_Attrs = Videoobject_Attrs
    { videoobjectId :: (Maybe String)
    , videoobjectLang :: (Maybe String)
    , videoobjectRevisionflag :: (Maybe Videoobject_Revisionflag)
    , videoobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Videoobject_Revisionflag = Videoobject_Revisionflag_Changed
				 |  Videoobject_Revisionflag_Added  | 
				Videoobject_Revisionflag_Deleted  | 
				Videoobject_Revisionflag_Off
			      deriving (Eq,Show)
data Audioobject = Audioobject Audioobject_Attrs
			       (Maybe Objectinfo) Audiodata
		 deriving (Eq,Show)
data Audioobject_Attrs = Audioobject_Attrs
    { audioobjectId :: (Maybe String)
    , audioobjectLang :: (Maybe String)
    , audioobjectRevisionflag :: (Maybe Audioobject_Revisionflag)
    , audioobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Audioobject_Revisionflag = Audioobject_Revisionflag_Changed
				 |  Audioobject_Revisionflag_Added  | 
				Audioobject_Revisionflag_Deleted  | 
				Audioobject_Revisionflag_Off
			      deriving (Eq,Show)
data Imageobject = Imageobject Imageobject_Attrs
			       (Maybe Objectinfo) Imagedata
		 deriving (Eq,Show)
data Imageobject_Attrs = Imageobject_Attrs
    { imageobjectId :: (Maybe String)
    , imageobjectLang :: (Maybe String)
    , imageobjectRevisionflag :: (Maybe Imageobject_Revisionflag)
    , imageobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Imageobject_Revisionflag = Imageobject_Revisionflag_Changed
				 |  Imageobject_Revisionflag_Added  | 
				Imageobject_Revisionflag_Deleted  | 
				Imageobject_Revisionflag_Off
			      deriving (Eq,Show)
data Textobject = Textobject Textobject_Attrs
			     (Maybe Objectinfo)
			     (OneOf3 Phrase Textdata (List1 (OneOf8 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote)))
		deriving (Eq,Show)
data Textobject_Attrs = Textobject_Attrs
    { textobjectId :: (Maybe String)
    , textobjectLang :: (Maybe String)
    , textobjectRevisionflag :: (Maybe Textobject_Revisionflag)
    , textobjectRole :: (Maybe String)
    } deriving (Eq,Show)
data Textobject_Revisionflag = Textobject_Revisionflag_Changed
			        |  Textobject_Revisionflag_Added  | 
			       Textobject_Revisionflag_Deleted  | 
			       Textobject_Revisionflag_Off
			     deriving (Eq,Show)
data Objectinfo = Objectinfo Objectinfo_Attrs
			     (List1 (OneOf32 Mediaobject Legalnotice Keywordset Subjectset Abbrev Abstract Author Authorgroup Bibliomisc Copyright Corpauthor Date Edition Editor Issuenum Othercredit Pubdate Publishername Releaseinfo Revhistory Subtitle Title Titleabbrev Volumenum Citetitle Honorific Firstname Surname Lineage Othername Affiliation Authorblurb))
		deriving (Eq,Show)
data Objectinfo_Attrs = Objectinfo_Attrs
    { objectinfoId :: (Maybe String)
    , objectinfoLang :: (Maybe String)
    , objectinfoRevisionflag :: (Maybe Objectinfo_Revisionflag)
    , objectinfoRole :: (Maybe String)
    } deriving (Eq,Show)
data Objectinfo_Revisionflag = Objectinfo_Revisionflag_Changed
			        |  Objectinfo_Revisionflag_Added  | 
			       Objectinfo_Revisionflag_Deleted  | 
			       Objectinfo_Revisionflag_Off
			     deriving (Eq,Show)
data Videodata = Videodata
    { videodataWidth :: (Maybe String)
    , videodataContentwidth :: (Maybe String)
    , videodataDepth :: (Maybe String)
    , videodataContentdepth :: (Maybe String)
    , videodataAlign :: (Maybe Videodata_Align)
    , videodataValign :: (Maybe Videodata_Valign)
    , videodataScale :: (Maybe String)
    , videodataScalefit :: (Maybe String)
    , videodataEntityref :: (Maybe String)
    , videodataFileref :: (Maybe String)
    , videodataFormat :: (Maybe Videodata_Format)
    , videodataSrccredit :: (Maybe String)
    , videodataId :: (Maybe String)
    , videodataLang :: (Maybe String)
    , videodataRevisionflag :: (Maybe Videodata_Revisionflag)
    , videodataRole :: (Maybe String)
    } deriving (Eq,Show)
data Videodata_Align = Videodata_Align_Left  | 
		       Videodata_Align_Right  |  Videodata_Align_Center
		     deriving (Eq,Show)
data Videodata_Valign = Videodata_Valign_Top  | 
			Videodata_Valign_Middle  |  Videodata_Valign_Bottom
		      deriving (Eq,Show)
data Videodata_Format = Videodata_Format_BMP  | 
			Videodata_Format_CGM_CHAR  | 
			Videodata_Format_CGM_BINARY  | 
			Videodata_Format_CGM_CLEAR  | 
			Videodata_Format_DITROFF  |  Videodata_Format_DVI  | 
			Videodata_Format_EPS  |  Videodata_Format_EQN  | 
			Videodata_Format_FAX  |  Videodata_Format_GIF  | 
			Videodata_Format_GIF87a  |  Videodata_Format_GIF89a
			 |  Videodata_Format_JPG  |  Videodata_Format_JPEG
			 |  Videodata_Format_IGES  |  Videodata_Format_PCX
			 |  Videodata_Format_PIC  |  Videodata_Format_PNG  | 
			Videodata_Format_PS  |  Videodata_Format_SGML  | 
			Videodata_Format_TBL  |  Videodata_Format_TEX  | 
			Videodata_Format_TIFF  |  Videodata_Format_WMF  | 
			Videodata_Format_WPG  |  Videodata_Format_SVG  | 
			Videodata_Format_Linespecific
		      deriving (Eq,Show)
data Videodata_Revisionflag = Videodata_Revisionflag_Changed
			       |  Videodata_Revisionflag_Added  | 
			      Videodata_Revisionflag_Deleted  | 
			      Videodata_Revisionflag_Off
			    deriving (Eq,Show)
data Audiodata = Audiodata
    { audiodataEntityref :: (Maybe String)
    , audiodataFileref :: (Maybe String)
    , audiodataFormat :: (Maybe Audiodata_Format)
    , audiodataSrccredit :: (Maybe String)
    , audiodataId :: (Maybe String)
    , audiodataLang :: (Maybe String)
    , audiodataRevisionflag :: (Maybe Audiodata_Revisionflag)
    , audiodataRole :: (Maybe String)
    } deriving (Eq,Show)
data Audiodata_Format = Audiodata_Format_BMP  | 
			Audiodata_Format_CGM_CHAR  | 
			Audiodata_Format_CGM_BINARY  | 
			Audiodata_Format_CGM_CLEAR  | 
			Audiodata_Format_DITROFF  |  Audiodata_Format_DVI  | 
			Audiodata_Format_EPS  |  Audiodata_Format_EQN  | 
			Audiodata_Format_FAX  |  Audiodata_Format_GIF  | 
			Audiodata_Format_GIF87a  |  Audiodata_Format_GIF89a
			 |  Audiodata_Format_JPG  |  Audiodata_Format_JPEG
			 |  Audiodata_Format_IGES  |  Audiodata_Format_PCX
			 |  Audiodata_Format_PIC  |  Audiodata_Format_PNG  | 
			Audiodata_Format_PS  |  Audiodata_Format_SGML  | 
			Audiodata_Format_TBL  |  Audiodata_Format_TEX  | 
			Audiodata_Format_TIFF  |  Audiodata_Format_WMF  | 
			Audiodata_Format_WPG  |  Audiodata_Format_SVG  | 
			Audiodata_Format_Linespecific
		      deriving (Eq,Show)
data Audiodata_Revisionflag = Audiodata_Revisionflag_Changed
			       |  Audiodata_Revisionflag_Added  | 
			      Audiodata_Revisionflag_Deleted  | 
			      Audiodata_Revisionflag_Off
			    deriving (Eq,Show)
data Imagedata = Imagedata
    { imagedataWidth :: (Maybe String)
    , imagedataContentwidth :: (Maybe String)
    , imagedataDepth :: (Maybe String)
    , imagedataContentdepth :: (Maybe String)
    , imagedataAlign :: (Maybe Imagedata_Align)
    , imagedataValign :: (Maybe Imagedata_Valign)
    , imagedataScale :: (Maybe String)
    , imagedataScalefit :: (Maybe String)
    , imagedataEntityref :: (Maybe String)
    , imagedataFileref :: (Maybe String)
    , imagedataFormat :: (Maybe Imagedata_Format)
    , imagedataSrccredit :: (Maybe String)
    , imagedataId :: (Maybe String)
    , imagedataLang :: (Maybe String)
    , imagedataRevisionflag :: (Maybe Imagedata_Revisionflag)
    , imagedataRole :: (Maybe String)
    } deriving (Eq,Show)
data Imagedata_Align = Imagedata_Align_Left  | 
		       Imagedata_Align_Right  |  Imagedata_Align_Center
		     deriving (Eq,Show)
data Imagedata_Valign = Imagedata_Valign_Top  | 
			Imagedata_Valign_Middle  |  Imagedata_Valign_Bottom
		      deriving (Eq,Show)
data Imagedata_Format = Imagedata_Format_BMP  | 
			Imagedata_Format_CGM_CHAR  | 
			Imagedata_Format_CGM_BINARY  | 
			Imagedata_Format_CGM_CLEAR  | 
			Imagedata_Format_DITROFF  |  Imagedata_Format_DVI  | 
			Imagedata_Format_EPS  |  Imagedata_Format_EQN  | 
			Imagedata_Format_FAX  |  Imagedata_Format_GIF  | 
			Imagedata_Format_GIF87a  |  Imagedata_Format_GIF89a
			 |  Imagedata_Format_JPG  |  Imagedata_Format_JPEG
			 |  Imagedata_Format_IGES  |  Imagedata_Format_PCX
			 |  Imagedata_Format_PIC  |  Imagedata_Format_PNG  | 
			Imagedata_Format_PS  |  Imagedata_Format_SGML  | 
			Imagedata_Format_TBL  |  Imagedata_Format_TEX  | 
			Imagedata_Format_TIFF  |  Imagedata_Format_WMF  | 
			Imagedata_Format_WPG  |  Imagedata_Format_SVG  | 
			Imagedata_Format_Linespecific
		      deriving (Eq,Show)
data Imagedata_Revisionflag = Imagedata_Revisionflag_Changed
			       |  Imagedata_Revisionflag_Added  | 
			      Imagedata_Revisionflag_Deleted  | 
			      Imagedata_Revisionflag_Off
			    deriving (Eq,Show)
data Textdata = Textdata
    { textdataEncoding :: (Maybe String)
    , textdataEntityref :: (Maybe String)
    , textdataFileref :: (Maybe String)
    , textdataFormat :: (Maybe Textdata_Format)
    , textdataSrccredit :: (Maybe String)
    , textdataId :: (Maybe String)
    , textdataLang :: (Maybe String)
    , textdataRevisionflag :: (Maybe Textdata_Revisionflag)
    , textdataRole :: (Maybe String)
    } deriving (Eq,Show)
data Textdata_Format = Textdata_Format_BMP  | 
		       Textdata_Format_CGM_CHAR  | 
		       Textdata_Format_CGM_BINARY  | 
		       Textdata_Format_CGM_CLEAR  |  Textdata_Format_DITROFF
		        |  Textdata_Format_DVI  |  Textdata_Format_EPS  | 
		       Textdata_Format_EQN  |  Textdata_Format_FAX  | 
		       Textdata_Format_GIF  |  Textdata_Format_GIF87a  | 
		       Textdata_Format_GIF89a  |  Textdata_Format_JPG  | 
		       Textdata_Format_JPEG  |  Textdata_Format_IGES  | 
		       Textdata_Format_PCX  |  Textdata_Format_PIC  | 
		       Textdata_Format_PNG  |  Textdata_Format_PS  | 
		       Textdata_Format_SGML  |  Textdata_Format_TBL  | 
		       Textdata_Format_TEX  |  Textdata_Format_TIFF  | 
		       Textdata_Format_WMF  |  Textdata_Format_WPG  | 
		       Textdata_Format_SVG  |  Textdata_Format_Linespecific
		     deriving (Eq,Show)
data Textdata_Revisionflag = Textdata_Revisionflag_Changed
			      |  Textdata_Revisionflag_Added  | 
			     Textdata_Revisionflag_Deleted  | 
			     Textdata_Revisionflag_Off
			   deriving (Eq,Show)
data Caption = Caption Caption_Attrs [Caption_]
	     deriving (Eq,Show)
data Caption_Attrs = Caption_Attrs
    { captionId :: (Maybe String)
    , captionLang :: (Maybe String)
    , captionRevisionflag :: (Maybe Caption_Revisionflag)
    , captionRole :: (Maybe String)
    } deriving (Eq,Show)
data Caption_ = Caption_Itemizedlist Itemizedlist
	      | Caption_Orderedlist Orderedlist
	      | Caption_Variablelist Variablelist
	      | Caption_Note Note
	      | Caption_Literallayout Literallayout
	      | Caption_Programlisting Programlisting
	      | Caption_Para Para
	      | Caption_Blockquote Blockquote
	      deriving (Eq,Show)
data Caption_Revisionflag = Caption_Revisionflag_Changed
			     |  Caption_Revisionflag_Added  | 
			    Caption_Revisionflag_Deleted  | 
			    Caption_Revisionflag_Off
			  deriving (Eq,Show)
data Table = Table Table_Attrs Title
		   (OneOf2 (List1 Mediaobject) (List1 Tgroup))
	   deriving (Eq,Show)
data Table_Attrs = Table_Attrs
    { tableFrame :: (Maybe Table_Frame)
    , tableColsep :: (Maybe String)
    , tableRowsep :: (Maybe String)
    , tableTabstyle :: (Maybe String)
    , tableTocentry :: (Maybe String)
    , tableShortentry :: (Maybe String)
    , tableOrient :: (Maybe Table_Orient)
    , tablePgwide :: (Maybe String)
    , tableLabel :: (Maybe String)
    , tableId :: (Maybe String)
    , tableLang :: (Maybe String)
    , tableRevisionflag :: (Maybe Table_Revisionflag)
    , tableRole :: (Maybe String)
    } deriving (Eq,Show)
data Table_Frame = Table_Frame_Top  | 
		   Table_Frame_Bottom  |  Table_Frame_Topbot  | 
		   Table_Frame_All  |  Table_Frame_Sides  | 
		   Table_Frame_None
		 deriving (Eq,Show)
data Table_Orient = Table_Orient_Port  | 
		    Table_Orient_Land
		  deriving (Eq,Show)
data Table_Revisionflag = Table_Revisionflag_Changed
			   |  Table_Revisionflag_Added  | 
			  Table_Revisionflag_Deleted  |  Table_Revisionflag_Off
			deriving (Eq,Show)
data Tgroup = Tgroup Tgroup_Attrs [Colspec]
		     [Spanspec] (Maybe Thead) (Maybe Tfoot) Tbody
	    deriving (Eq,Show)
data Tgroup_Attrs = Tgroup_Attrs
    { tgroupCols :: String
    , tgroupTgroupstyle :: (Maybe String)
    , tgroupColsep :: (Maybe String)
    , tgroupRowsep :: (Maybe String)
    , tgroupAlign :: (Maybe Tgroup_Align)
    , tgroupChar :: (Maybe String)
    , tgroupCharoff :: (Maybe String)
    , tgroupId :: (Maybe String)
    , tgroupLang :: (Maybe String)
    , tgroupRevisionflag :: (Maybe Tgroup_Revisionflag)
    , tgroupRole :: (Maybe String)
    } deriving (Eq,Show)
data Tgroup_Align = Tgroup_Align_Left  | 
		    Tgroup_Align_Right  |  Tgroup_Align_Center  | 
		    Tgroup_Align_Justify  |  Tgroup_Align_Char
		  deriving (Eq,Show)
data Tgroup_Revisionflag = Tgroup_Revisionflag_Changed
			    |  Tgroup_Revisionflag_Added  | 
			   Tgroup_Revisionflag_Deleted  | 
			   Tgroup_Revisionflag_Off
			 deriving (Eq,Show)
data Colspec = Colspec
    { colspecColnum :: (Maybe String)
    , colspecColname :: (Maybe String)
    , colspecColwidth :: (Maybe String)
    , colspecColsep :: (Maybe String)
    , colspecRowsep :: (Maybe String)
    , colspecAlign :: (Maybe Colspec_Align)
    , colspecChar :: (Maybe String)
    , colspecCharoff :: (Maybe String)
    } deriving (Eq,Show)
data Colspec_Align = Colspec_Align_Left  | 
		     Colspec_Align_Right  |  Colspec_Align_Center  | 
		     Colspec_Align_Justify  |  Colspec_Align_Char
		   deriving (Eq,Show)
data Spanspec = Spanspec
    { spanspecNamest :: String
    , spanspecNameend :: String
    , spanspecSpanname :: String
    , spanspecColsep :: (Maybe String)
    , spanspecRowsep :: (Maybe String)
    , spanspecAlign :: (Maybe Spanspec_Align)
    , spanspecChar :: (Maybe String)
    , spanspecCharoff :: (Maybe String)
    } deriving (Eq,Show)
data Spanspec_Align = Spanspec_Align_Left  | 
		      Spanspec_Align_Right  |  Spanspec_Align_Center  | 
		      Spanspec_Align_Justify  |  Spanspec_Align_Char
		    deriving (Eq,Show)
data Thead = Thead Thead_Attrs [Colspec] (List1 Row)
	   deriving (Eq,Show)
data Thead_Attrs = Thead_Attrs
    { theadValign :: (Maybe Thead_Valign)
    , theadId :: (Maybe String)
    , theadLang :: (Maybe String)
    , theadRevisionflag :: (Maybe Thead_Revisionflag)
    , theadRole :: (Maybe String)
    } deriving (Eq,Show)
data Thead_Valign = Thead_Valign_Top  | 
		    Thead_Valign_Middle  |  Thead_Valign_Bottom
		  deriving (Eq,Show)
data Thead_Revisionflag = Thead_Revisionflag_Changed
			   |  Thead_Revisionflag_Added  | 
			  Thead_Revisionflag_Deleted  |  Thead_Revisionflag_Off
			deriving (Eq,Show)
data Tfoot = Tfoot Tfoot_Attrs [Colspec] (List1 Row)
	   deriving (Eq,Show)
data Tfoot_Attrs = Tfoot_Attrs
    { tfootValign :: (Maybe Tfoot_Valign)
    , tfootId :: (Maybe String)
    , tfootLang :: (Maybe String)
    , tfootRevisionflag :: (Maybe Tfoot_Revisionflag)
    , tfootRole :: (Maybe String)
    } deriving (Eq,Show)
data Tfoot_Valign = Tfoot_Valign_Top  | 
		    Tfoot_Valign_Middle  |  Tfoot_Valign_Bottom
		  deriving (Eq,Show)
data Tfoot_Revisionflag = Tfoot_Revisionflag_Changed
			   |  Tfoot_Revisionflag_Added  | 
			  Tfoot_Revisionflag_Deleted  |  Tfoot_Revisionflag_Off
			deriving (Eq,Show)
data Tbody = Tbody Tbody_Attrs (List1 Row)
	   deriving (Eq,Show)
data Tbody_Attrs = Tbody_Attrs
    { tbodyValign :: (Maybe Tbody_Valign)
    , tbodyId :: (Maybe String)
    , tbodyLang :: (Maybe String)
    , tbodyRevisionflag :: (Maybe Tbody_Revisionflag)
    , tbodyRole :: (Maybe String)
    } deriving (Eq,Show)
data Tbody_Valign = Tbody_Valign_Top  | 
		    Tbody_Valign_Middle  |  Tbody_Valign_Bottom
		  deriving (Eq,Show)
data Tbody_Revisionflag = Tbody_Revisionflag_Changed
			   |  Tbody_Revisionflag_Added  | 
			  Tbody_Revisionflag_Deleted  |  Tbody_Revisionflag_Off
			deriving (Eq,Show)
data Row = Row Row_Attrs
	       (List1 (OneOf2 Entry Entrytbl))
	 deriving (Eq,Show)
data Row_Attrs = Row_Attrs
    { rowRowsep :: (Maybe String)
    , rowValign :: (Maybe Row_Valign)
    , rowId :: (Maybe String)
    , rowLang :: (Maybe String)
    , rowRevisionflag :: (Maybe Row_Revisionflag)
    , rowRole :: (Maybe String)
    } deriving (Eq,Show)
data Row_Valign = Row_Valign_Top  | 
		  Row_Valign_Middle  |  Row_Valign_Bottom
		deriving (Eq,Show)
data Row_Revisionflag = Row_Revisionflag_Changed  | 
			Row_Revisionflag_Added  |  Row_Revisionflag_Deleted
			 |  Row_Revisionflag_Off
		      deriving (Eq,Show)
data Entrytbl = Entrytbl Entrytbl_Attrs [Colspec]
			 [Spanspec] (Maybe Thead) Tbody
	      deriving (Eq,Show)
data Entrytbl_Attrs = Entrytbl_Attrs
    { entrytblCols :: String
    , entrytblTgroupstyle :: (Maybe String)
    , entrytblColname :: (Maybe String)
    , entrytblSpanname :: (Maybe String)
    , entrytblNamest :: (Maybe String)
    , entrytblNameend :: (Maybe String)
    , entrytblColsep :: (Maybe String)
    , entrytblRowsep :: (Maybe String)
    , entrytblAlign :: (Maybe Entrytbl_Align)
    , entrytblChar :: (Maybe String)
    , entrytblCharoff :: (Maybe String)
    , entrytblId :: (Maybe String)
    , entrytblLang :: (Maybe String)
    , entrytblRevisionflag :: (Maybe Entrytbl_Revisionflag)
    , entrytblRole :: (Maybe String)
    } deriving (Eq,Show)
data Entrytbl_Align = Entrytbl_Align_Left  | 
		      Entrytbl_Align_Right  |  Entrytbl_Align_Center  | 
		      Entrytbl_Align_Justify  |  Entrytbl_Align_Char
		    deriving (Eq,Show)
data Entrytbl_Revisionflag = Entrytbl_Revisionflag_Changed
			      |  Entrytbl_Revisionflag_Added  | 
			     Entrytbl_Revisionflag_Deleted  | 
			     Entrytbl_Revisionflag_Off
			   deriving (Eq,Show)
data Entry = Entry Entry_Attrs [Entry_]
	   deriving (Eq,Show)
data Entry_Attrs = Entry_Attrs
    { entryColname :: (Maybe String)
    , entryNamest :: (Maybe String)
    , entryNameend :: (Maybe String)
    , entrySpanname :: (Maybe String)
    , entryMorerows :: (Maybe String)
    , entryColsep :: (Maybe String)
    , entryRowsep :: (Maybe String)
    , entryAlign :: (Maybe Entry_Align)
    , entryChar :: (Maybe String)
    , entryCharoff :: (Maybe String)
    , entryRotate :: (Maybe String)
    , entryValign :: (Maybe Entry_Valign)
    , entryId :: (Maybe String)
    , entryLang :: (Maybe String)
    , entryRevisionflag :: (Maybe Entry_Revisionflag)
    , entryRole :: (Maybe String)
    } deriving (Eq,Show)
data Entry_ = Entry_Str String
	    | Entry_Footnoteref Footnoteref
	    | Entry_Xref Xref
	    | Entry_Abbrev Abbrev
	    | Entry_Acronym Acronym
	    | Entry_Citetitle Citetitle
	    | Entry_Emphasis Emphasis
	    | Entry_Footnote Footnote
	    | Entry_Phrase Phrase
	    | Entry_Quote Quote
	    | Entry_Trademark Trademark
	    | Entry_Link Link
	    | Entry_Ulink Ulink
	    | Entry_Command Command
	    | Entry_Computeroutput Computeroutput
	    | Entry_Email Email
	    | Entry_Filename Filename
	    | Entry_Literal Literal
	    | Entry_Option Option
	    | Entry_Replaceable Replaceable
	    | Entry_Systemitem Systemitem
	    | Entry_Userinput Userinput
	    | Entry_Inlinemediaobject Inlinemediaobject
	    | Entry_Itemizedlist Itemizedlist
	    | Entry_Orderedlist Orderedlist
	    | Entry_Variablelist Variablelist
	    | Entry_Note Note
	    | Entry_Literallayout Literallayout
	    | Entry_Programlisting Programlisting
	    | Entry_Para Para
	    | Entry_Mediaobject Mediaobject
	    deriving (Eq,Show)
data Entry_Align = Entry_Align_Left  | 
		   Entry_Align_Right  |  Entry_Align_Center  | 
		   Entry_Align_Justify  |  Entry_Align_Char
		 deriving (Eq,Show)
data Entry_Valign = Entry_Valign_Top  | 
		    Entry_Valign_Middle  |  Entry_Valign_Bottom
		  deriving (Eq,Show)
data Entry_Revisionflag = Entry_Revisionflag_Changed
			   |  Entry_Revisionflag_Added  | 
			  Entry_Revisionflag_Deleted  |  Entry_Revisionflag_Off
			deriving (Eq,Show)
data Informaltable = InformaltableMediaobject Informaltable_Attrs
					      (List1 Mediaobject)
		   | InformaltableTgroup Informaltable_Attrs
					 (List1 Tgroup)
		   deriving (Eq,Show)
data Informaltable_Attrs = Informaltable_Attrs
    { informaltableFrame :: (Maybe Informaltable_Frame)
    , informaltableColsep :: (Maybe String)
    , informaltableRowsep :: (Maybe String)
    , informaltableLabel :: (Maybe String)
    , informaltableId :: (Maybe String)
    , informaltableLang :: (Maybe String)
    , informaltableRevisionflag :: (Maybe Informaltable_Revisionflag)
    , informaltableRole :: (Maybe String)
    , informaltableTabstyle :: (Maybe String)
    , informaltableTocentry :: (Maybe String)
    , informaltableShortentry :: (Maybe String)
    , informaltableOrient :: (Maybe Informaltable_Orient)
    , informaltablePgwide :: (Maybe String)
    } deriving (Eq,Show)
data Informaltable_Frame = Informaltable_Frame_Top
			    |  Informaltable_Frame_Bottom  | 
			   Informaltable_Frame_Topbot  | 
			   Informaltable_Frame_All  |  Informaltable_Frame_Sides
			    |  Informaltable_Frame_None
			 deriving (Eq,Show)
data Informaltable_Revisionflag = Informaltable_Revisionflag_Changed
				   |  Informaltable_Revisionflag_Added  | 
				  Informaltable_Revisionflag_Deleted  | 
				  Informaltable_Revisionflag_Off
				deriving (Eq,Show)
data Informaltable_Orient = Informaltable_Orient_Port
			     |  Informaltable_Orient_Land
			  deriving (Eq,Show)
data Affiliation = Affiliation Affiliation_Attrs
			       (Maybe Jobtitle) (Maybe Orgname)
		 deriving (Eq,Show)
data Affiliation_Attrs = Affiliation_Attrs
    { affiliationId :: (Maybe String)
    , affiliationLang :: (Maybe String)
    , affiliationRevisionflag :: (Maybe Affiliation_Revisionflag)
    , affiliationRole :: (Maybe String)
    } deriving (Eq,Show)
data Affiliation_Revisionflag = Affiliation_Revisionflag_Changed
				 |  Affiliation_Revisionflag_Added  | 
				Affiliation_Revisionflag_Deleted  | 
				Affiliation_Revisionflag_Off
			      deriving (Eq,Show)
data Jobtitle = Jobtitle Jobtitle_Attrs [Jobtitle_]
	      deriving (Eq,Show)
data Jobtitle_Attrs = Jobtitle_Attrs
    { jobtitleId :: (Maybe String)
    , jobtitleLang :: (Maybe String)
    , jobtitleRevisionflag :: (Maybe Jobtitle_Revisionflag)
    , jobtitleRole :: (Maybe String)
    } deriving (Eq,Show)
data Jobtitle_ = Jobtitle_Str String
	       | Jobtitle_Link Link
	       | Jobtitle_Ulink Ulink
	       | Jobtitle_Emphasis Emphasis
	       | Jobtitle_Trademark Trademark
	       | Jobtitle_Replaceable Replaceable
	       | Jobtitle_Inlinemediaobject Inlinemediaobject
	       deriving (Eq,Show)
data Jobtitle_Revisionflag = Jobtitle_Revisionflag_Changed
			      |  Jobtitle_Revisionflag_Added  | 
			     Jobtitle_Revisionflag_Deleted  | 
			     Jobtitle_Revisionflag_Off
			   deriving (Eq,Show)
data Author = Author Author_Attrs
		     (List1 (OneOf7 Honorific Firstname Surname Lineage Othername Affiliation Authorblurb))
	    deriving (Eq,Show)
data Author_Attrs = Author_Attrs
    { authorId :: (Maybe String)
    , authorLang :: (Maybe String)
    , authorRevisionflag :: (Maybe Author_Revisionflag)
    , authorRole :: (Maybe String)
    } deriving (Eq,Show)
data Author_Revisionflag = Author_Revisionflag_Changed
			    |  Author_Revisionflag_Added  | 
			   Author_Revisionflag_Deleted  | 
			   Author_Revisionflag_Off
			 deriving (Eq,Show)
data Authorgroup = Authorgroup Authorgroup_Attrs
			       (List1 (OneOf4 Author Editor Corpauthor Othercredit))
		 deriving (Eq,Show)
data Authorgroup_Attrs = Authorgroup_Attrs
    { authorgroupId :: (Maybe String)
    , authorgroupLang :: (Maybe String)
    , authorgroupRevisionflag :: (Maybe Authorgroup_Revisionflag)
    , authorgroupRole :: (Maybe String)
    } deriving (Eq,Show)
data Authorgroup_Revisionflag = Authorgroup_Revisionflag_Changed
				 |  Authorgroup_Revisionflag_Added  | 
				Authorgroup_Revisionflag_Deleted  | 
				Authorgroup_Revisionflag_Off
			      deriving (Eq,Show)
data Authorinitials = Authorinitials Authorinitials_Attrs
				     [Authorinitials_]
		    deriving (Eq,Show)
data Authorinitials_Attrs = Authorinitials_Attrs
    { authorinitialsId :: (Maybe String)
    , authorinitialsLang :: (Maybe String)
    , authorinitialsRevisionflag :: (Maybe Authorinitials_Revisionflag)
    , authorinitialsRole :: (Maybe String)
    } deriving (Eq,Show)
data Authorinitials_ = Authorinitials_Str String
		     | Authorinitials_Link Link
		     | Authorinitials_Ulink Ulink
		     | Authorinitials_Emphasis Emphasis
		     | Authorinitials_Trademark Trademark
		     | Authorinitials_Replaceable Replaceable
		     | Authorinitials_Inlinemediaobject Inlinemediaobject
		     deriving (Eq,Show)
data Authorinitials_Revisionflag = Authorinitials_Revisionflag_Changed
				    |  Authorinitials_Revisionflag_Added  | 
				   Authorinitials_Revisionflag_Deleted  | 
				   Authorinitials_Revisionflag_Off
				 deriving (Eq,Show)
data Copyright = Copyright Copyright_Attrs
			   (List1 Year) [Holder]
	       deriving (Eq,Show)
data Copyright_Attrs = Copyright_Attrs
    { copyrightId :: (Maybe String)
    , copyrightLang :: (Maybe String)
    , copyrightRevisionflag :: (Maybe Copyright_Revisionflag)
    , copyrightRole :: (Maybe String)
    } deriving (Eq,Show)
data Copyright_Revisionflag = Copyright_Revisionflag_Changed
			       |  Copyright_Revisionflag_Added  | 
			      Copyright_Revisionflag_Deleted  | 
			      Copyright_Revisionflag_Off
			    deriving (Eq,Show)
data Year = Year Year_Attrs [Year_]
	  deriving (Eq,Show)
data Year_Attrs = Year_Attrs
    { yearId :: (Maybe String)
    , yearLang :: (Maybe String)
    , yearRevisionflag :: (Maybe Year_Revisionflag)
    , yearRole :: (Maybe String)
    } deriving (Eq,Show)
data Year_ = Year_Str String
	   | Year_Link Link
	   | Year_Ulink Ulink
	   | Year_Emphasis Emphasis
	   | Year_Trademark Trademark
	   | Year_Replaceable Replaceable
	   | Year_Inlinemediaobject Inlinemediaobject
	   deriving (Eq,Show)
data Year_Revisionflag = Year_Revisionflag_Changed
			  |  Year_Revisionflag_Added  | 
			 Year_Revisionflag_Deleted  |  Year_Revisionflag_Off
		       deriving (Eq,Show)
data Holder = Holder Holder_Attrs [Holder_]
	    deriving (Eq,Show)
data Holder_Attrs = Holder_Attrs
    { holderId :: (Maybe String)
    , holderLang :: (Maybe String)
    , holderRevisionflag :: (Maybe Holder_Revisionflag)
    , holderRole :: (Maybe String)
    } deriving (Eq,Show)
data Holder_ = Holder_Str String
	     | Holder_Link Link
	     | Holder_Ulink Ulink
	     | Holder_Emphasis Emphasis
	     | Holder_Trademark Trademark
	     | Holder_Replaceable Replaceable
	     | Holder_Inlinemediaobject Inlinemediaobject
	     deriving (Eq,Show)
data Holder_Revisionflag = Holder_Revisionflag_Changed
			    |  Holder_Revisionflag_Added  | 
			   Holder_Revisionflag_Deleted  | 
			   Holder_Revisionflag_Off
			 deriving (Eq,Show)
data Corpauthor = Corpauthor Corpauthor_Attrs
			     [Corpauthor_]
		deriving (Eq,Show)
data Corpauthor_Attrs = Corpauthor_Attrs
    { corpauthorId :: (Maybe String)
    , corpauthorLang :: (Maybe String)
    , corpauthorRevisionflag :: (Maybe Corpauthor_Revisionflag)
    , corpauthorRole :: (Maybe String)
    } deriving (Eq,Show)
data Corpauthor_ = Corpauthor_Str String
		 | Corpauthor_Link Link
		 | Corpauthor_Ulink Ulink
		 | Corpauthor_Emphasis Emphasis
		 | Corpauthor_Trademark Trademark
		 | Corpauthor_Replaceable Replaceable
		 | Corpauthor_Inlinemediaobject Inlinemediaobject
		 deriving (Eq,Show)
data Corpauthor_Revisionflag = Corpauthor_Revisionflag_Changed
			        |  Corpauthor_Revisionflag_Added  | 
			       Corpauthor_Revisionflag_Deleted  | 
			       Corpauthor_Revisionflag_Off
			     deriving (Eq,Show)
data Date = Date Date_Attrs [Date_]
	  deriving (Eq,Show)
data Date_Attrs = Date_Attrs
    { dateId :: (Maybe String)
    , dateLang :: (Maybe String)
    , dateRevisionflag :: (Maybe Date_Revisionflag)
    , dateRole :: (Maybe String)
    } deriving (Eq,Show)
data Date_ = Date_Str String
	   | Date_Link Link
	   | Date_Ulink Ulink
	   | Date_Emphasis Emphasis
	   | Date_Trademark Trademark
	   | Date_Replaceable Replaceable
	   | Date_Inlinemediaobject Inlinemediaobject
	   deriving (Eq,Show)
data Date_Revisionflag = Date_Revisionflag_Changed
			  |  Date_Revisionflag_Added  | 
			 Date_Revisionflag_Deleted  |  Date_Revisionflag_Off
		       deriving (Eq,Show)
data Edition = Edition Edition_Attrs [Edition_]
	     deriving (Eq,Show)
data Edition_Attrs = Edition_Attrs
    { editionId :: (Maybe String)
    , editionLang :: (Maybe String)
    , editionRevisionflag :: (Maybe Edition_Revisionflag)
    , editionRole :: (Maybe String)
    } deriving (Eq,Show)
data Edition_ = Edition_Str String
	      | Edition_Link Link
	      | Edition_Ulink Ulink
	      | Edition_Emphasis Emphasis
	      | Edition_Trademark Trademark
	      | Edition_Replaceable Replaceable
	      | Edition_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Edition_Revisionflag = Edition_Revisionflag_Changed
			     |  Edition_Revisionflag_Added  | 
			    Edition_Revisionflag_Deleted  | 
			    Edition_Revisionflag_Off
			  deriving (Eq,Show)
data Editor = Editor Editor_Attrs
		     (List1 (OneOf7 Honorific Firstname Surname Lineage Othername Affiliation Authorblurb))
	    deriving (Eq,Show)
data Editor_Attrs = Editor_Attrs
    { editorId :: (Maybe String)
    , editorLang :: (Maybe String)
    , editorRevisionflag :: (Maybe Editor_Revisionflag)
    , editorRole :: (Maybe String)
    } deriving (Eq,Show)
data Editor_Revisionflag = Editor_Revisionflag_Changed
			    |  Editor_Revisionflag_Added  | 
			   Editor_Revisionflag_Deleted  | 
			   Editor_Revisionflag_Off
			 deriving (Eq,Show)
data Issuenum = Issuenum Issuenum_Attrs [Issuenum_]
	      deriving (Eq,Show)
data Issuenum_Attrs = Issuenum_Attrs
    { issuenumId :: (Maybe String)
    , issuenumLang :: (Maybe String)
    , issuenumRevisionflag :: (Maybe Issuenum_Revisionflag)
    , issuenumRole :: (Maybe String)
    } deriving (Eq,Show)
data Issuenum_ = Issuenum_Str String
	       | Issuenum_Link Link
	       | Issuenum_Ulink Ulink
	       | Issuenum_Emphasis Emphasis
	       | Issuenum_Trademark Trademark
	       | Issuenum_Replaceable Replaceable
	       | Issuenum_Inlinemediaobject Inlinemediaobject
	       deriving (Eq,Show)
data Issuenum_Revisionflag = Issuenum_Revisionflag_Changed
			      |  Issuenum_Revisionflag_Added  | 
			     Issuenum_Revisionflag_Deleted  | 
			     Issuenum_Revisionflag_Off
			   deriving (Eq,Show)
data Legalnotice = Legalnotice Legalnotice_Attrs
			       (Maybe Title)
			       (List1 (OneOf8 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote))
		 deriving (Eq,Show)
data Legalnotice_Attrs = Legalnotice_Attrs
    { legalnoticeId :: (Maybe String)
    , legalnoticeLang :: (Maybe String)
    , legalnoticeRevisionflag :: (Maybe Legalnotice_Revisionflag)
    , legalnoticeRole :: (Maybe String)
    } deriving (Eq,Show)
data Legalnotice_Revisionflag = Legalnotice_Revisionflag_Changed
				 |  Legalnotice_Revisionflag_Added  | 
				Legalnotice_Revisionflag_Deleted  | 
				Legalnotice_Revisionflag_Off
			      deriving (Eq,Show)
data Orgname = Orgname Orgname_Attrs [Orgname_]
	     deriving (Eq,Show)
data Orgname_Attrs = Orgname_Attrs
    { orgnameId :: (Maybe String)
    , orgnameLang :: (Maybe String)
    , orgnameRevisionflag :: (Maybe Orgname_Revisionflag)
    , orgnameClass :: (Maybe Orgname_Class)
    , orgnameOtherclass :: (Maybe String)
    , orgnameRole :: (Maybe String)
    } deriving (Eq,Show)
data Orgname_ = Orgname_Str String
	      | Orgname_Link Link
	      | Orgname_Ulink Ulink
	      | Orgname_Emphasis Emphasis
	      | Orgname_Trademark Trademark
	      | Orgname_Replaceable Replaceable
	      | Orgname_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Orgname_Revisionflag = Orgname_Revisionflag_Changed
			     |  Orgname_Revisionflag_Added  | 
			    Orgname_Revisionflag_Deleted  | 
			    Orgname_Revisionflag_Off
			  deriving (Eq,Show)
data Orgname_Class = Orgname_Class_Corporation  | 
		     Orgname_Class_Nonprofit  |  Orgname_Class_Consortium
		      |  Orgname_Class_Informal  |  Orgname_Class_Other
		   deriving (Eq,Show)
data Othercredit = Othercredit Othercredit_Attrs
			       (List1 (OneOf7 Honorific Firstname Surname Lineage Othername Affiliation Authorblurb))
		 deriving (Eq,Show)
data Othercredit_Attrs = Othercredit_Attrs
    { othercreditId :: (Maybe String)
    , othercreditLang :: (Maybe String)
    , othercreditRevisionflag :: (Maybe Othercredit_Revisionflag)
    , othercreditRole :: (Maybe String)
    } deriving (Eq,Show)
data Othercredit_Revisionflag = Othercredit_Revisionflag_Changed
				 |  Othercredit_Revisionflag_Added  | 
				Othercredit_Revisionflag_Deleted  | 
				Othercredit_Revisionflag_Off
			      deriving (Eq,Show)
data Firstname = Firstname Firstname_Attrs
			   [Firstname_]
	       deriving (Eq,Show)
data Firstname_Attrs = Firstname_Attrs
    { firstnameId :: (Maybe String)
    , firstnameLang :: (Maybe String)
    , firstnameRevisionflag :: (Maybe Firstname_Revisionflag)
    , firstnameRole :: (Maybe String)
    } deriving (Eq,Show)
data Firstname_ = Firstname_Str String
		| Firstname_Link Link
		| Firstname_Ulink Ulink
		| Firstname_Emphasis Emphasis
		| Firstname_Trademark Trademark
		| Firstname_Replaceable Replaceable
		| Firstname_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Firstname_Revisionflag = Firstname_Revisionflag_Changed
			       |  Firstname_Revisionflag_Added  | 
			      Firstname_Revisionflag_Deleted  | 
			      Firstname_Revisionflag_Off
			    deriving (Eq,Show)
data Honorific = Honorific Honorific_Attrs
			   [Honorific_]
	       deriving (Eq,Show)
data Honorific_Attrs = Honorific_Attrs
    { honorificId :: (Maybe String)
    , honorificLang :: (Maybe String)
    , honorificRevisionflag :: (Maybe Honorific_Revisionflag)
    , honorificRole :: (Maybe String)
    } deriving (Eq,Show)
data Honorific_ = Honorific_Str String
		| Honorific_Link Link
		| Honorific_Ulink Ulink
		| Honorific_Emphasis Emphasis
		| Honorific_Trademark Trademark
		| Honorific_Replaceable Replaceable
		| Honorific_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Honorific_Revisionflag = Honorific_Revisionflag_Changed
			       |  Honorific_Revisionflag_Added  | 
			      Honorific_Revisionflag_Deleted  | 
			      Honorific_Revisionflag_Off
			    deriving (Eq,Show)
data Lineage = Lineage Lineage_Attrs [Lineage_]
	     deriving (Eq,Show)
data Lineage_Attrs = Lineage_Attrs
    { lineageId :: (Maybe String)
    , lineageLang :: (Maybe String)
    , lineageRevisionflag :: (Maybe Lineage_Revisionflag)
    , lineageRole :: (Maybe String)
    } deriving (Eq,Show)
data Lineage_ = Lineage_Str String
	      | Lineage_Link Link
	      | Lineage_Ulink Ulink
	      | Lineage_Emphasis Emphasis
	      | Lineage_Trademark Trademark
	      | Lineage_Replaceable Replaceable
	      | Lineage_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Lineage_Revisionflag = Lineage_Revisionflag_Changed
			     |  Lineage_Revisionflag_Added  | 
			    Lineage_Revisionflag_Deleted  | 
			    Lineage_Revisionflag_Off
			  deriving (Eq,Show)
data Othername = Othername Othername_Attrs
			   [Othername_]
	       deriving (Eq,Show)
data Othername_Attrs = Othername_Attrs
    { othernameId :: (Maybe String)
    , othernameLang :: (Maybe String)
    , othernameRevisionflag :: (Maybe Othername_Revisionflag)
    , othernameRole :: (Maybe String)
    } deriving (Eq,Show)
data Othername_ = Othername_Str String
		| Othername_Link Link
		| Othername_Ulink Ulink
		| Othername_Emphasis Emphasis
		| Othername_Trademark Trademark
		| Othername_Replaceable Replaceable
		| Othername_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Othername_Revisionflag = Othername_Revisionflag_Changed
			       |  Othername_Revisionflag_Added  | 
			      Othername_Revisionflag_Deleted  | 
			      Othername_Revisionflag_Off
			    deriving (Eq,Show)
data Surname = Surname Surname_Attrs [Surname_]
	     deriving (Eq,Show)
data Surname_Attrs = Surname_Attrs
    { surnameId :: (Maybe String)
    , surnameLang :: (Maybe String)
    , surnameRevisionflag :: (Maybe Surname_Revisionflag)
    , surnameRole :: (Maybe String)
    } deriving (Eq,Show)
data Surname_ = Surname_Str String
	      | Surname_Link Link
	      | Surname_Ulink Ulink
	      | Surname_Emphasis Emphasis
	      | Surname_Trademark Trademark
	      | Surname_Replaceable Replaceable
	      | Surname_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Surname_Revisionflag = Surname_Revisionflag_Changed
			     |  Surname_Revisionflag_Added  | 
			    Surname_Revisionflag_Deleted  | 
			    Surname_Revisionflag_Off
			  deriving (Eq,Show)
data Pubdate = Pubdate Pubdate_Attrs [Pubdate_]
	     deriving (Eq,Show)
data Pubdate_Attrs = Pubdate_Attrs
    { pubdateId :: (Maybe String)
    , pubdateLang :: (Maybe String)
    , pubdateRevisionflag :: (Maybe Pubdate_Revisionflag)
    , pubdateRole :: (Maybe String)
    } deriving (Eq,Show)
data Pubdate_ = Pubdate_Str String
	      | Pubdate_Link Link
	      | Pubdate_Ulink Ulink
	      | Pubdate_Emphasis Emphasis
	      | Pubdate_Trademark Trademark
	      | Pubdate_Replaceable Replaceable
	      | Pubdate_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Pubdate_Revisionflag = Pubdate_Revisionflag_Changed
			     |  Pubdate_Revisionflag_Added  | 
			    Pubdate_Revisionflag_Deleted  | 
			    Pubdate_Revisionflag_Off
			  deriving (Eq,Show)
data Publishername = Publishername Publishername_Attrs
				   [Publishername_]
		   deriving (Eq,Show)
data Publishername_Attrs = Publishername_Attrs
    { publishernameId :: (Maybe String)
    , publishernameLang :: (Maybe String)
    , publishernameRevisionflag :: (Maybe Publishername_Revisionflag)
    , publishernameRole :: (Maybe String)
    } deriving (Eq,Show)
data Publishername_ = Publishername_Str String
		    | Publishername_Link Link
		    | Publishername_Ulink Ulink
		    | Publishername_Emphasis Emphasis
		    | Publishername_Trademark Trademark
		    | Publishername_Replaceable Replaceable
		    | Publishername_Inlinemediaobject Inlinemediaobject
		    deriving (Eq,Show)
data Publishername_Revisionflag = Publishername_Revisionflag_Changed
				   |  Publishername_Revisionflag_Added  | 
				  Publishername_Revisionflag_Deleted  | 
				  Publishername_Revisionflag_Off
				deriving (Eq,Show)
data Releaseinfo = Releaseinfo Releaseinfo_Attrs
			       [Releaseinfo_]
		 deriving (Eq,Show)
data Releaseinfo_Attrs = Releaseinfo_Attrs
    { releaseinfoId :: (Maybe String)
    , releaseinfoLang :: (Maybe String)
    , releaseinfoRevisionflag :: (Maybe Releaseinfo_Revisionflag)
    , releaseinfoRole :: (Maybe String)
    } deriving (Eq,Show)
data Releaseinfo_ = Releaseinfo_Str String
		  | Releaseinfo_Link Link
		  | Releaseinfo_Ulink Ulink
		  | Releaseinfo_Emphasis Emphasis
		  | Releaseinfo_Trademark Trademark
		  | Releaseinfo_Replaceable Replaceable
		  | Releaseinfo_Inlinemediaobject Inlinemediaobject
		  deriving (Eq,Show)
data Releaseinfo_Revisionflag = Releaseinfo_Revisionflag_Changed
				 |  Releaseinfo_Revisionflag_Added  | 
				Releaseinfo_Revisionflag_Deleted  | 
				Releaseinfo_Revisionflag_Off
			      deriving (Eq,Show)
data Revhistory = Revhistory Revhistory_Attrs
			     (List1 Revision)
		deriving (Eq,Show)
data Revhistory_Attrs = Revhistory_Attrs
    { revhistoryId :: (Maybe String)
    , revhistoryLang :: (Maybe String)
    , revhistoryRevisionflag :: (Maybe Revhistory_Revisionflag)
    , revhistoryRole :: (Maybe String)
    } deriving (Eq,Show)
data Revhistory_Revisionflag = Revhistory_Revisionflag_Changed
			        |  Revhistory_Revisionflag_Added  | 
			       Revhistory_Revisionflag_Deleted  | 
			       Revhistory_Revisionflag_Off
			     deriving (Eq,Show)
data Revision = Revision Revision_Attrs Revnumber
			 Date [Authorinitials]
			 (Maybe (OneOf2 Revremark Revdescription))
	      deriving (Eq,Show)
data Revision_Attrs = Revision_Attrs
    { revisionId :: (Maybe String)
    , revisionLang :: (Maybe String)
    , revisionRevisionflag :: (Maybe Revision_Revisionflag)
    , revisionRole :: (Maybe String)
    } deriving (Eq,Show)
data Revision_Revisionflag = Revision_Revisionflag_Changed
			      |  Revision_Revisionflag_Added  | 
			     Revision_Revisionflag_Deleted  | 
			     Revision_Revisionflag_Off
			   deriving (Eq,Show)
data Revnumber = Revnumber Revnumber_Attrs
			   [Revnumber_]
	       deriving (Eq,Show)
data Revnumber_Attrs = Revnumber_Attrs
    { revnumberId :: (Maybe String)
    , revnumberLang :: (Maybe String)
    , revnumberRevisionflag :: (Maybe Revnumber_Revisionflag)
    , revnumberRole :: (Maybe String)
    } deriving (Eq,Show)
data Revnumber_ = Revnumber_Str String
		| Revnumber_Link Link
		| Revnumber_Ulink Ulink
		| Revnumber_Emphasis Emphasis
		| Revnumber_Trademark Trademark
		| Revnumber_Replaceable Replaceable
		| Revnumber_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Revnumber_Revisionflag = Revnumber_Revisionflag_Changed
			       |  Revnumber_Revisionflag_Added  | 
			      Revnumber_Revisionflag_Deleted  | 
			      Revnumber_Revisionflag_Off
			    deriving (Eq,Show)
data Revremark = Revremark Revremark_Attrs
			   [Revremark_]
	       deriving (Eq,Show)
data Revremark_Attrs = Revremark_Attrs
    { revremarkId :: (Maybe String)
    , revremarkLang :: (Maybe String)
    , revremarkRevisionflag :: (Maybe Revremark_Revisionflag)
    , revremarkRole :: (Maybe String)
    } deriving (Eq,Show)
data Revremark_ = Revremark_Str String
		| Revremark_Link Link
		| Revremark_Ulink Ulink
		| Revremark_Emphasis Emphasis
		| Revremark_Trademark Trademark
		| Revremark_Replaceable Replaceable
		| Revremark_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Revremark_Revisionflag = Revremark_Revisionflag_Changed
			       |  Revremark_Revisionflag_Added  | 
			      Revremark_Revisionflag_Deleted  | 
			      Revremark_Revisionflag_Off
			    deriving (Eq,Show)
data Revdescription = Revdescription Revdescription_Attrs
				     (List1 (OneOf13 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table))
		    deriving (Eq,Show)
data Revdescription_Attrs = Revdescription_Attrs
    { revdescriptionId :: (Maybe String)
    , revdescriptionLang :: (Maybe String)
    , revdescriptionRevisionflag :: (Maybe Revdescription_Revisionflag)
    , revdescriptionRole :: (Maybe String)
    } deriving (Eq,Show)
data Revdescription_Revisionflag = Revdescription_Revisionflag_Changed
				    |  Revdescription_Revisionflag_Added  | 
				   Revdescription_Revisionflag_Deleted  | 
				   Revdescription_Revisionflag_Off
				 deriving (Eq,Show)
data Volumenum = Volumenum Volumenum_Attrs
			   [Volumenum_]
	       deriving (Eq,Show)
data Volumenum_Attrs = Volumenum_Attrs
    { volumenumId :: (Maybe String)
    , volumenumLang :: (Maybe String)
    , volumenumRevisionflag :: (Maybe Volumenum_Revisionflag)
    , volumenumRole :: (Maybe String)
    } deriving (Eq,Show)
data Volumenum_ = Volumenum_Str String
		| Volumenum_Link Link
		| Volumenum_Ulink Ulink
		| Volumenum_Emphasis Emphasis
		| Volumenum_Trademark Trademark
		| Volumenum_Replaceable Replaceable
		| Volumenum_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Volumenum_Revisionflag = Volumenum_Revisionflag_Changed
			       |  Volumenum_Revisionflag_Added  | 
			      Volumenum_Revisionflag_Deleted  | 
			      Volumenum_Revisionflag_Off
			    deriving (Eq,Show)
data Command = Command Command_Attrs [Command_]
	     deriving (Eq,Show)
data Command_Attrs = Command_Attrs
    { commandMoreinfo :: (Defaultable Command_Moreinfo)
    , commandId :: (Maybe String)
    , commandLang :: (Maybe String)
    , commandRevisionflag :: (Maybe Command_Revisionflag)
    , commandRole :: (Maybe String)
    } deriving (Eq,Show)
data Command_ = Command_Str String
	      | Command_Link Link
	      | Command_Ulink Ulink
	      | Command_Command Command
	      | Command_Computeroutput Computeroutput
	      | Command_Email Email
	      | Command_Filename Filename
	      | Command_Literal Literal
	      | Command_Option Option
	      | Command_Replaceable Replaceable
	      | Command_Systemitem Systemitem
	      | Command_Userinput Userinput
	      | Command_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Command_Moreinfo = Command_Moreinfo_Refentry  | 
			Command_Moreinfo_None
		      deriving (Eq,Show)
data Command_Revisionflag = Command_Revisionflag_Changed
			     |  Command_Revisionflag_Added  | 
			    Command_Revisionflag_Deleted  | 
			    Command_Revisionflag_Off
			  deriving (Eq,Show)
data Computeroutput = Computeroutput Computeroutput_Attrs
				     [Computeroutput_]
		    deriving (Eq,Show)
data Computeroutput_Attrs = Computeroutput_Attrs
    { computeroutputMoreinfo :: (Defaultable Computeroutput_Moreinfo)
    , computeroutputId :: (Maybe String)
    , computeroutputLang :: (Maybe String)
    , computeroutputRevisionflag :: (Maybe Computeroutput_Revisionflag)
    , computeroutputRole :: (Maybe String)
    } deriving (Eq,Show)
data Computeroutput_ = Computeroutput_Str String
		     | Computeroutput_Link Link
		     | Computeroutput_Ulink Ulink
		     | Computeroutput_Command Command
		     | Computeroutput_Computeroutput Computeroutput
		     | Computeroutput_Email Email
		     | Computeroutput_Filename Filename
		     | Computeroutput_Literal Literal
		     | Computeroutput_Option Option
		     | Computeroutput_Replaceable Replaceable
		     | Computeroutput_Systemitem Systemitem
		     | Computeroutput_Userinput Userinput
		     | Computeroutput_Inlinemediaobject Inlinemediaobject
		     deriving (Eq,Show)
data Computeroutput_Moreinfo = Computeroutput_Moreinfo_Refentry
			        |  Computeroutput_Moreinfo_None
			     deriving (Eq,Show)
data Computeroutput_Revisionflag = Computeroutput_Revisionflag_Changed
				    |  Computeroutput_Revisionflag_Added  | 
				   Computeroutput_Revisionflag_Deleted  | 
				   Computeroutput_Revisionflag_Off
				 deriving (Eq,Show)
data Email = Email Email_Attrs [Email_]
	   deriving (Eq,Show)
data Email_Attrs = Email_Attrs
    { emailId :: (Maybe String)
    , emailLang :: (Maybe String)
    , emailRevisionflag :: (Maybe Email_Revisionflag)
    , emailRole :: (Maybe String)
    } deriving (Eq,Show)
data Email_ = Email_Str String
	    | Email_Link Link
	    | Email_Ulink Ulink
	    | Email_Emphasis Emphasis
	    | Email_Trademark Trademark
	    | Email_Replaceable Replaceable
	    | Email_Inlinemediaobject Inlinemediaobject
	    deriving (Eq,Show)
data Email_Revisionflag = Email_Revisionflag_Changed
			   |  Email_Revisionflag_Added  | 
			  Email_Revisionflag_Deleted  |  Email_Revisionflag_Off
			deriving (Eq,Show)
data Filename = Filename Filename_Attrs [Filename_]
	      deriving (Eq,Show)
data Filename_Attrs = Filename_Attrs
    { filenameClass :: (Maybe Filename_Class)
    , filenamePath :: (Maybe String)
    , filenameMoreinfo :: (Defaultable Filename_Moreinfo)
    , filenameId :: (Maybe String)
    , filenameLang :: (Maybe String)
    , filenameRevisionflag :: (Maybe Filename_Revisionflag)
    , filenameRole :: (Maybe String)
    } deriving (Eq,Show)
data Filename_ = Filename_Str String
	       | Filename_Link Link
	       | Filename_Ulink Ulink
	       | Filename_Command Command
	       | Filename_Computeroutput Computeroutput
	       | Filename_Email Email
	       | Filename_Filename Filename
	       | Filename_Literal Literal
	       | Filename_Option Option
	       | Filename_Replaceable Replaceable
	       | Filename_Systemitem Systemitem
	       | Filename_Userinput Userinput
	       | Filename_Inlinemediaobject Inlinemediaobject
	       deriving (Eq,Show)
data Filename_Class = Filename_Class_Headerfile  | 
		      Filename_Class_Partition  | 
		      Filename_Class_Devicefile  | 
		      Filename_Class_Libraryfile  | 
		      Filename_Class_Directory  |  Filename_Class_Extension
		       |  Filename_Class_Symlink
		    deriving (Eq,Show)
data Filename_Moreinfo = Filename_Moreinfo_Refentry
			  |  Filename_Moreinfo_None
		       deriving (Eq,Show)
data Filename_Revisionflag = Filename_Revisionflag_Changed
			      |  Filename_Revisionflag_Added  | 
			     Filename_Revisionflag_Deleted  | 
			     Filename_Revisionflag_Off
			   deriving (Eq,Show)
data Lineannotation = Lineannotation Lineannotation_Attrs
				     [Lineannotation_]
		    deriving (Eq,Show)
data Lineannotation_Attrs = Lineannotation_Attrs
    { lineannotationId :: (Maybe String)
    , lineannotationLang :: (Maybe String)
    , lineannotationRevisionflag :: (Maybe Lineannotation_Revisionflag)
    , lineannotationRole :: (Maybe String)
    } deriving (Eq,Show)
data Lineannotation_ = Lineannotation_Str String
		     | Lineannotation_Footnoteref Footnoteref
		     | Lineannotation_Xref Xref
		     | Lineannotation_Abbrev Abbrev
		     | Lineannotation_Acronym Acronym
		     | Lineannotation_Citetitle Citetitle
		     | Lineannotation_Emphasis Emphasis
		     | Lineannotation_Footnote Footnote
		     | Lineannotation_Phrase Phrase
		     | Lineannotation_Quote Quote
		     | Lineannotation_Trademark Trademark
		     | Lineannotation_Link Link
		     | Lineannotation_Ulink Ulink
		     | Lineannotation_Command Command
		     | Lineannotation_Computeroutput Computeroutput
		     | Lineannotation_Email Email
		     | Lineannotation_Filename Filename
		     | Lineannotation_Literal Literal
		     | Lineannotation_Option Option
		     | Lineannotation_Replaceable Replaceable
		     | Lineannotation_Systemitem Systemitem
		     | Lineannotation_Userinput Userinput
		     | Lineannotation_Inlinemediaobject Inlinemediaobject
		     deriving (Eq,Show)
data Lineannotation_Revisionflag = Lineannotation_Revisionflag_Changed
				    |  Lineannotation_Revisionflag_Added  | 
				   Lineannotation_Revisionflag_Deleted  | 
				   Lineannotation_Revisionflag_Off
				 deriving (Eq,Show)
data Literal = Literal Literal_Attrs [Literal_]
	     deriving (Eq,Show)
data Literal_Attrs = Literal_Attrs
    { literalMoreinfo :: (Defaultable Literal_Moreinfo)
    , literalId :: (Maybe String)
    , literalLang :: (Maybe String)
    , literalRevisionflag :: (Maybe Literal_Revisionflag)
    , literalRole :: (Maybe String)
    } deriving (Eq,Show)
data Literal_ = Literal_Str String
	      | Literal_Link Link
	      | Literal_Ulink Ulink
	      | Literal_Command Command
	      | Literal_Computeroutput Computeroutput
	      | Literal_Email Email
	      | Literal_Filename Filename
	      | Literal_Literal Literal
	      | Literal_Option Option
	      | Literal_Replaceable Replaceable
	      | Literal_Systemitem Systemitem
	      | Literal_Userinput Userinput
	      | Literal_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Literal_Moreinfo = Literal_Moreinfo_Refentry  | 
			Literal_Moreinfo_None
		      deriving (Eq,Show)
data Literal_Revisionflag = Literal_Revisionflag_Changed
			     |  Literal_Revisionflag_Added  | 
			    Literal_Revisionflag_Deleted  | 
			    Literal_Revisionflag_Off
			  deriving (Eq,Show)
data Option = Option Option_Attrs [Option_]
	    deriving (Eq,Show)
data Option_Attrs = Option_Attrs
    { optionId :: (Maybe String)
    , optionLang :: (Maybe String)
    , optionRevisionflag :: (Maybe Option_Revisionflag)
    , optionRole :: (Maybe String)
    } deriving (Eq,Show)
data Option_ = Option_Str String
	     | Option_Link Link
	     | Option_Ulink Ulink
	     | Option_Command Command
	     | Option_Computeroutput Computeroutput
	     | Option_Email Email
	     | Option_Filename Filename
	     | Option_Literal Literal
	     | Option_Option Option
	     | Option_Replaceable Replaceable
	     | Option_Systemitem Systemitem
	     | Option_Userinput Userinput
	     | Option_Inlinemediaobject Inlinemediaobject
	     deriving (Eq,Show)
data Option_Revisionflag = Option_Revisionflag_Changed
			    |  Option_Revisionflag_Added  | 
			   Option_Revisionflag_Deleted  | 
			   Option_Revisionflag_Off
			 deriving (Eq,Show)
data Replaceable = Replaceable Replaceable_Attrs
			       [Replaceable_]
		 deriving (Eq,Show)
data Replaceable_Attrs = Replaceable_Attrs
    { replaceableClass :: (Maybe Replaceable_Class)
    , replaceableId :: (Maybe String)
    , replaceableLang :: (Maybe String)
    , replaceableRevisionflag :: (Maybe Replaceable_Revisionflag)
    , replaceableRole :: (Maybe String)
    } deriving (Eq,Show)
data Replaceable_ = Replaceable_Str String
		  | Replaceable_Link Link
		  | Replaceable_Ulink Ulink
		  | Replaceable_Inlinemediaobject Inlinemediaobject
		  deriving (Eq,Show)
data Replaceable_Class = Replaceable_Class_Command
			  |  Replaceable_Class_Function  | 
			 Replaceable_Class_Option  | 
			 Replaceable_Class_Parameter
		       deriving (Eq,Show)
data Replaceable_Revisionflag = Replaceable_Revisionflag_Changed
				 |  Replaceable_Revisionflag_Added  | 
				Replaceable_Revisionflag_Deleted  | 
				Replaceable_Revisionflag_Off
			      deriving (Eq,Show)
data Systemitem = Systemitem Systemitem_Attrs
			     [Systemitem_]
		deriving (Eq,Show)
data Systemitem_Attrs = Systemitem_Attrs
    { systemitemClass :: (Maybe Systemitem_Class)
    , systemitemMoreinfo :: (Defaultable Systemitem_Moreinfo)
    , systemitemId :: (Maybe String)
    , systemitemLang :: (Maybe String)
    , systemitemRevisionflag :: (Maybe Systemitem_Revisionflag)
    , systemitemRole :: (Maybe String)
    } deriving (Eq,Show)
data Systemitem_ = Systemitem_Str String
		 | Systemitem_Link Link
		 | Systemitem_Ulink Ulink
		 | Systemitem_Command Command
		 | Systemitem_Computeroutput Computeroutput
		 | Systemitem_Email Email
		 | Systemitem_Filename Filename
		 | Systemitem_Literal Literal
		 | Systemitem_Option Option
		 | Systemitem_Replaceable Replaceable
		 | Systemitem_Systemitem Systemitem
		 | Systemitem_Userinput Userinput
		 | Systemitem_Inlinemediaobject Inlinemediaobject
		 | Systemitem_Acronym Acronym
		 deriving (Eq,Show)
data Systemitem_Class = Systemitem_Class_Constant  | 
			Systemitem_Class_Event  | 
			Systemitem_Class_Eventhandler  | 
			Systemitem_Class_Domainname  | 
			Systemitem_Class_Fqdomainname  | 
			Systemitem_Class_Ipaddress  | 
			Systemitem_Class_Netmask  | 
			Systemitem_Class_Etheraddress  | 
			Systemitem_Class_Groupname  | 
			Systemitem_Class_Library  |  Systemitem_Class_Macro
			 |  Systemitem_Class_Osname  | 
			Systemitem_Class_Filesystem  | 
			Systemitem_Class_Resource  | 
			Systemitem_Class_Systemname  | 
			Systemitem_Class_Username  | 
			Systemitem_Class_Newsgroup
		      deriving (Eq,Show)
data Systemitem_Moreinfo = Systemitem_Moreinfo_Refentry
			    |  Systemitem_Moreinfo_None
			 deriving (Eq,Show)
data Systemitem_Revisionflag = Systemitem_Revisionflag_Changed
			        |  Systemitem_Revisionflag_Added  | 
			       Systemitem_Revisionflag_Deleted  | 
			       Systemitem_Revisionflag_Off
			     deriving (Eq,Show)
data Userinput = Userinput Userinput_Attrs
			   [Userinput_]
	       deriving (Eq,Show)
data Userinput_Attrs = Userinput_Attrs
    { userinputMoreinfo :: (Defaultable Userinput_Moreinfo)
    , userinputId :: (Maybe String)
    , userinputLang :: (Maybe String)
    , userinputRevisionflag :: (Maybe Userinput_Revisionflag)
    , userinputRole :: (Maybe String)
    } deriving (Eq,Show)
data Userinput_ = Userinput_Str String
		| Userinput_Link Link
		| Userinput_Ulink Ulink
		| Userinput_Command Command
		| Userinput_Computeroutput Computeroutput
		| Userinput_Email Email
		| Userinput_Filename Filename
		| Userinput_Literal Literal
		| Userinput_Option Option
		| Userinput_Replaceable Replaceable
		| Userinput_Systemitem Systemitem
		| Userinput_Userinput Userinput
		| Userinput_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Userinput_Moreinfo = Userinput_Moreinfo_Refentry
			   |  Userinput_Moreinfo_None
			deriving (Eq,Show)
data Userinput_Revisionflag = Userinput_Revisionflag_Changed
			       |  Userinput_Revisionflag_Added  | 
			      Userinput_Revisionflag_Deleted  | 
			      Userinput_Revisionflag_Off
			    deriving (Eq,Show)
data Abbrev = Abbrev Abbrev_Attrs [Abbrev_]
	    deriving (Eq,Show)
data Abbrev_Attrs = Abbrev_Attrs
    { abbrevId :: (Maybe String)
    , abbrevLang :: (Maybe String)
    , abbrevRevisionflag :: (Maybe Abbrev_Revisionflag)
    , abbrevRole :: (Maybe String)
    } deriving (Eq,Show)
data Abbrev_ = Abbrev_Str String
	     | Abbrev_Acronym Acronym
	     | Abbrev_Emphasis Emphasis
	     | Abbrev_Trademark Trademark
	     | Abbrev_Link Link
	     | Abbrev_Ulink Ulink
	     | Abbrev_Inlinemediaobject Inlinemediaobject
	     deriving (Eq,Show)
data Abbrev_Revisionflag = Abbrev_Revisionflag_Changed
			    |  Abbrev_Revisionflag_Added  | 
			   Abbrev_Revisionflag_Deleted  | 
			   Abbrev_Revisionflag_Off
			 deriving (Eq,Show)
data Acronym = Acronym Acronym_Attrs [Acronym_]
	     deriving (Eq,Show)
data Acronym_Attrs = Acronym_Attrs
    { acronymId :: (Maybe String)
    , acronymLang :: (Maybe String)
    , acronymRevisionflag :: (Maybe Acronym_Revisionflag)
    , acronymRole :: (Maybe String)
    } deriving (Eq,Show)
data Acronym_ = Acronym_Str String
	      | Acronym_Acronym Acronym
	      | Acronym_Emphasis Emphasis
	      | Acronym_Trademark Trademark
	      | Acronym_Link Link
	      | Acronym_Ulink Ulink
	      | Acronym_Inlinemediaobject Inlinemediaobject
	      deriving (Eq,Show)
data Acronym_Revisionflag = Acronym_Revisionflag_Changed
			     |  Acronym_Revisionflag_Added  | 
			    Acronym_Revisionflag_Deleted  | 
			    Acronym_Revisionflag_Off
			  deriving (Eq,Show)
data Citetitle = Citetitle Citetitle_Attrs
			   [Citetitle_]
	       deriving (Eq,Show)
data Citetitle_Attrs = Citetitle_Attrs
    { citetitlePubwork :: (Maybe Citetitle_Pubwork)
    , citetitleId :: (Maybe String)
    , citetitleLang :: (Maybe String)
    , citetitleRevisionflag :: (Maybe Citetitle_Revisionflag)
    , citetitleRole :: (Maybe String)
    } deriving (Eq,Show)
data Citetitle_ = Citetitle_Str String
		| Citetitle_Footnoteref Footnoteref
		| Citetitle_Xref Xref
		| Citetitle_Abbrev Abbrev
		| Citetitle_Acronym Acronym
		| Citetitle_Citetitle Citetitle
		| Citetitle_Emphasis Emphasis
		| Citetitle_Footnote Footnote
		| Citetitle_Phrase Phrase
		| Citetitle_Quote Quote
		| Citetitle_Trademark Trademark
		| Citetitle_Link Link
		| Citetitle_Ulink Ulink
		| Citetitle_Command Command
		| Citetitle_Computeroutput Computeroutput
		| Citetitle_Email Email
		| Citetitle_Filename Filename
		| Citetitle_Literal Literal
		| Citetitle_Option Option
		| Citetitle_Replaceable Replaceable
		| Citetitle_Systemitem Systemitem
		| Citetitle_Userinput Userinput
		| Citetitle_Inlinemediaobject Inlinemediaobject
		deriving (Eq,Show)
data Citetitle_Pubwork = Citetitle_Pubwork_Article
			  |  Citetitle_Pubwork_Book  | 
			 Citetitle_Pubwork_Chapter  |  Citetitle_Pubwork_Part
			  |  Citetitle_Pubwork_Refentry  | 
			 Citetitle_Pubwork_Section  | 
			 Citetitle_Pubwork_Journal  | 
			 Citetitle_Pubwork_Series  |  Citetitle_Pubwork_Set
			  |  Citetitle_Pubwork_Manuscript
		       deriving (Eq,Show)
data Citetitle_Revisionflag = Citetitle_Revisionflag_Changed
			       |  Citetitle_Revisionflag_Added  | 
			      Citetitle_Revisionflag_Deleted  | 
			      Citetitle_Revisionflag_Off
			    deriving (Eq,Show)
data Emphasis = Emphasis Emphasis_Attrs [Emphasis_]
	      deriving (Eq,Show)
data Emphasis_Attrs = Emphasis_Attrs
    { emphasisId :: (Maybe String)
    , emphasisLang :: (Maybe String)
    , emphasisRevisionflag :: (Maybe Emphasis_Revisionflag)
    , emphasisRole :: (Maybe String)
    } deriving (Eq,Show)
data Emphasis_ = Emphasis_Str String
	       | Emphasis_Footnoteref Footnoteref
	       | Emphasis_Xref Xref
	       | Emphasis_Abbrev Abbrev
	       | Emphasis_Acronym Acronym
	       | Emphasis_Citetitle Citetitle
	       | Emphasis_Emphasis Emphasis
	       | Emphasis_Footnote Footnote
	       | Emphasis_Phrase Phrase
	       | Emphasis_Quote Quote
	       | Emphasis_Trademark Trademark
	       | Emphasis_Link Link
	       | Emphasis_Ulink Ulink
	       | Emphasis_Command Command
	       | Emphasis_Computeroutput Computeroutput
	       | Emphasis_Email Email
	       | Emphasis_Filename Filename
	       | Emphasis_Literal Literal
	       | Emphasis_Option Option
	       | Emphasis_Replaceable Replaceable
	       | Emphasis_Systemitem Systemitem
	       | Emphasis_Userinput Userinput
	       | Emphasis_Inlinemediaobject Inlinemediaobject
	       deriving (Eq,Show)
data Emphasis_Revisionflag = Emphasis_Revisionflag_Changed
			      |  Emphasis_Revisionflag_Added  | 
			     Emphasis_Revisionflag_Deleted  | 
			     Emphasis_Revisionflag_Off
			   deriving (Eq,Show)
data Phrase = Phrase Phrase_Attrs [Phrase_]
	    deriving (Eq,Show)
data Phrase_Attrs = Phrase_Attrs
    { phraseId :: (Maybe String)
    , phraseLang :: (Maybe String)
    , phraseRevisionflag :: (Maybe Phrase_Revisionflag)
    , phraseRole :: (Maybe String)
    } deriving (Eq,Show)
data Phrase_ = Phrase_Str String
	     | Phrase_Footnoteref Footnoteref
	     | Phrase_Xref Xref
	     | Phrase_Abbrev Abbrev
	     | Phrase_Acronym Acronym
	     | Phrase_Citetitle Citetitle
	     | Phrase_Emphasis Emphasis
	     | Phrase_Footnote Footnote
	     | Phrase_Phrase Phrase
	     | Phrase_Quote Quote
	     | Phrase_Trademark Trademark
	     | Phrase_Link Link
	     | Phrase_Ulink Ulink
	     | Phrase_Command Command
	     | Phrase_Computeroutput Computeroutput
	     | Phrase_Email Email
	     | Phrase_Filename Filename
	     | Phrase_Literal Literal
	     | Phrase_Option Option
	     | Phrase_Replaceable Replaceable
	     | Phrase_Systemitem Systemitem
	     | Phrase_Userinput Userinput
	     | Phrase_Inlinemediaobject Inlinemediaobject
	     deriving (Eq,Show)
data Phrase_Revisionflag = Phrase_Revisionflag_Changed
			    |  Phrase_Revisionflag_Added  | 
			   Phrase_Revisionflag_Deleted  | 
			   Phrase_Revisionflag_Off
			 deriving (Eq,Show)
data Quote = Quote Quote_Attrs [Quote_]
	   deriving (Eq,Show)
data Quote_Attrs = Quote_Attrs
    { quoteId :: (Maybe String)
    , quoteLang :: (Maybe String)
    , quoteRevisionflag :: (Maybe Quote_Revisionflag)
    , quoteRole :: (Maybe String)
    } deriving (Eq,Show)
data Quote_ = Quote_Str String
	    | Quote_Footnoteref Footnoteref
	    | Quote_Xref Xref
	    | Quote_Abbrev Abbrev
	    | Quote_Acronym Acronym
	    | Quote_Citetitle Citetitle
	    | Quote_Emphasis Emphasis
	    | Quote_Footnote Footnote
	    | Quote_Phrase Phrase
	    | Quote_Quote Quote
	    | Quote_Trademark Trademark
	    | Quote_Link Link
	    | Quote_Ulink Ulink
	    | Quote_Command Command
	    | Quote_Computeroutput Computeroutput
	    | Quote_Email Email
	    | Quote_Filename Filename
	    | Quote_Literal Literal
	    | Quote_Option Option
	    | Quote_Replaceable Replaceable
	    | Quote_Systemitem Systemitem
	    | Quote_Userinput Userinput
	    | Quote_Inlinemediaobject Inlinemediaobject
	    deriving (Eq,Show)
data Quote_Revisionflag = Quote_Revisionflag_Changed
			   |  Quote_Revisionflag_Added  | 
			  Quote_Revisionflag_Deleted  |  Quote_Revisionflag_Off
			deriving (Eq,Show)
data Trademark = Trademark Trademark_Attrs
			   [Trademark_]
	       deriving (Eq,Show)
data Trademark_Attrs = Trademark_Attrs
    { trademarkClass :: (Defaultable Trademark_Class)
    , trademarkId :: (Maybe String)
    , trademarkLang :: (Maybe String)
    , trademarkRevisionflag :: (Maybe Trademark_Revisionflag)
    , trademarkRole :: (Maybe String)
    } deriving (Eq,Show)
data Trademark_ = Trademark_Str String
		| Trademark_Link Link
		| Trademark_Ulink Ulink
		| Trademark_Command Command
		| Trademark_Computeroutput Computeroutput
		| Trademark_Email Email
		| Trademark_Filename Filename
		| Trademark_Literal Literal
		| Trademark_Option Option
		| Trademark_Replaceable Replaceable
		| Trademark_Systemitem Systemitem
		| Trademark_Userinput Userinput
		| Trademark_Inlinemediaobject Inlinemediaobject
		| Trademark_Emphasis Emphasis
		deriving (Eq,Show)
data Trademark_Class = Trademark_Class_Service  | 
		       Trademark_Class_Trade  |  Trademark_Class_Registered
		        |  Trademark_Class_Copyright
		     deriving (Eq,Show)
data Trademark_Revisionflag = Trademark_Revisionflag_Changed
			       |  Trademark_Revisionflag_Added  | 
			      Trademark_Revisionflag_Deleted  | 
			      Trademark_Revisionflag_Off
			    deriving (Eq,Show)
data Link = Link Link_Attrs [Link_]
	  deriving (Eq,Show)
data Link_Attrs = Link_Attrs
    { linkEndterm :: (Maybe String)
    , linkLinkend :: String
    , linkType :: (Maybe String)
    , linkId :: (Maybe String)
    , linkLang :: (Maybe String)
    , linkRevisionflag :: (Maybe Link_Revisionflag)
    , linkRole :: (Maybe String)
    } deriving (Eq,Show)
data Link_ = Link_Str String
	   | Link_Footnoteref Footnoteref
	   | Link_Xref Xref
	   | Link_Abbrev Abbrev
	   | Link_Acronym Acronym
	   | Link_Citetitle Citetitle
	   | Link_Emphasis Emphasis
	   | Link_Footnote Footnote
	   | Link_Phrase Phrase
	   | Link_Quote Quote
	   | Link_Trademark Trademark
	   | Link_Link Link
	   | Link_Ulink Ulink
	   | Link_Command Command
	   | Link_Computeroutput Computeroutput
	   | Link_Email Email
	   | Link_Filename Filename
	   | Link_Literal Literal
	   | Link_Option Option
	   | Link_Replaceable Replaceable
	   | Link_Systemitem Systemitem
	   | Link_Userinput Userinput
	   | Link_Inlinemediaobject Inlinemediaobject
	   deriving (Eq,Show)
data Link_Revisionflag = Link_Revisionflag_Changed
			  |  Link_Revisionflag_Added  | 
			 Link_Revisionflag_Deleted  |  Link_Revisionflag_Off
		       deriving (Eq,Show)
data Ulink = Ulink Ulink_Attrs [Ulink_]
	   deriving (Eq,Show)
data Ulink_Attrs = Ulink_Attrs
    { ulinkUrl :: String
    , ulinkType :: (Maybe String)
    , ulinkId :: (Maybe String)
    , ulinkLang :: (Maybe String)
    , ulinkRevisionflag :: (Maybe Ulink_Revisionflag)
    , ulinkRole :: (Maybe String)
    } deriving (Eq,Show)
data Ulink_ = Ulink_Str String
	    | Ulink_Footnoteref Footnoteref
	    | Ulink_Xref Xref
	    | Ulink_Abbrev Abbrev
	    | Ulink_Acronym Acronym
	    | Ulink_Citetitle Citetitle
	    | Ulink_Emphasis Emphasis
	    | Ulink_Footnote Footnote
	    | Ulink_Phrase Phrase
	    | Ulink_Quote Quote
	    | Ulink_Trademark Trademark
	    | Ulink_Link Link
	    | Ulink_Ulink Ulink
	    | Ulink_Command Command
	    | Ulink_Computeroutput Computeroutput
	    | Ulink_Email Email
	    | Ulink_Filename Filename
	    | Ulink_Literal Literal
	    | Ulink_Option Option
	    | Ulink_Replaceable Replaceable
	    | Ulink_Systemitem Systemitem
	    | Ulink_Userinput Userinput
	    | Ulink_Inlinemediaobject Inlinemediaobject
	    deriving (Eq,Show)
data Ulink_Revisionflag = Ulink_Revisionflag_Changed
			   |  Ulink_Revisionflag_Added  | 
			  Ulink_Revisionflag_Deleted  |  Ulink_Revisionflag_Off
			deriving (Eq,Show)
data Footnoteref = Footnoteref
    { footnoterefLinkend :: String
    , footnoterefLabel :: (Maybe String)
    , footnoterefId :: (Maybe String)
    , footnoterefLang :: (Maybe String)
    , footnoterefRevisionflag :: (Maybe Footnoteref_Revisionflag)
    , footnoterefRole :: (Maybe String)
    } deriving (Eq,Show)
data Footnoteref_Revisionflag = Footnoteref_Revisionflag_Changed
				 |  Footnoteref_Revisionflag_Added  | 
				Footnoteref_Revisionflag_Deleted  | 
				Footnoteref_Revisionflag_Off
			      deriving (Eq,Show)
data Xref = Xref
    { xrefEndterm :: (Maybe String)
    , xrefLinkend :: String
    , xrefId :: (Maybe String)
    , xrefLang :: (Maybe String)
    , xrefRevisionflag :: (Maybe Xref_Revisionflag)
    , xrefRole :: (Maybe String)
    } deriving (Eq,Show)
data Xref_Revisionflag = Xref_Revisionflag_Changed
			  |  Xref_Revisionflag_Added  | 
			 Xref_Revisionflag_Deleted  |  Xref_Revisionflag_Off
		       deriving (Eq,Show)
data Appendix = Appendix Appendix_Attrs Title
			 (Maybe Subtitle) (Maybe Titleabbrev)
			 (OneOf3 (List1 (OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph)) [Section] (List1 Section))
	      deriving (Eq,Show)
data Appendix_Attrs = Appendix_Attrs
    { appendixLabel :: (Maybe String)
    , appendixStatus :: (Maybe String)
    , appendixId :: (Maybe String)
    , appendixLang :: (Maybe String)
    , appendixRevisionflag :: (Maybe Appendix_Revisionflag)
    , appendixRole :: (Maybe String)
    } deriving (Eq,Show)
data Appendix_Revisionflag = Appendix_Revisionflag_Changed
			      |  Appendix_Revisionflag_Added  | 
			     Appendix_Revisionflag_Deleted  | 
			     Appendix_Revisionflag_Off
			   deriving (Eq,Show)
data Section = Section Section_Attrs
		       (Maybe Sectioninfo) Title (Maybe Subtitle)
		       (Maybe Titleabbrev)
		       (OneOf3 (List1 (OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph)) [Section] (List1 Section))
	     deriving (Eq,Show)
data Section_Attrs = Section_Attrs
    { sectionLabel :: (Maybe String)
    , sectionStatus :: (Maybe String)
    , sectionId :: (Maybe String)
    , sectionLang :: (Maybe String)
    , sectionRevisionflag :: (Maybe Section_Revisionflag)
    , sectionRole :: (Maybe String)
    } deriving (Eq,Show)
data Section_Revisionflag = Section_Revisionflag_Changed
			     |  Section_Revisionflag_Added  | 
			    Section_Revisionflag_Deleted  | 
			    Section_Revisionflag_Off
			  deriving (Eq,Show)
data Bibliography = Bibliography Bibliography_Attrs
				 (Maybe (Title,(Maybe Subtitle),(Maybe Titleabbrev)))
				 [(OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph)]
				 (OneOf2 (List1 Bibliodiv) (List1 Bibliomixed))
		  deriving (Eq,Show)
data Bibliography_Attrs = Bibliography_Attrs
    { bibliographyStatus :: (Maybe String)
    , bibliographyId :: (Maybe String)
    , bibliographyLang :: (Maybe String)
    , bibliographyRevisionflag :: (Maybe Bibliography_Revisionflag)
    , bibliographyRole :: (Maybe String)
    } deriving (Eq,Show)
data Bibliography_Revisionflag = Bibliography_Revisionflag_Changed
				  |  Bibliography_Revisionflag_Added  | 
				 Bibliography_Revisionflag_Deleted  | 
				 Bibliography_Revisionflag_Off
			       deriving (Eq,Show)
data Bibliodiv = Bibliodiv Bibliodiv_Attrs
			   (Maybe (Title,(Maybe Subtitle),(Maybe Titleabbrev)))
			   [(OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph)]
			   (List1 (Bibliomixed))
	       deriving (Eq,Show)
data Bibliodiv_Attrs = Bibliodiv_Attrs
    { bibliodivStatus :: (Maybe String)
    , bibliodivId :: (Maybe String)
    , bibliodivLang :: (Maybe String)
    , bibliodivRevisionflag :: (Maybe Bibliodiv_Revisionflag)
    , bibliodivRole :: (Maybe String)
    } deriving (Eq,Show)
data Bibliodiv_Revisionflag = Bibliodiv_Revisionflag_Changed
			       |  Bibliodiv_Revisionflag_Added  | 
			      Bibliodiv_Revisionflag_Deleted  | 
			      Bibliodiv_Revisionflag_Off
			    deriving (Eq,Show)
data Article = Article Article_Attrs
		       (Maybe (Title,(Maybe Subtitle),(Maybe Titleabbrev)))
		       (Maybe Articleinfo)
		       (OneOf3 (List1 (OneOf17 Itemizedlist Orderedlist Variablelist Note Literallayout Programlisting Para Blockquote Mediaobject Informaltable Example Figure Table Sidebar Abstract Authorblurb Epigraph)) [Section] (List1 Section))
		       [(OneOf2 Appendix Bibliography)]
	     deriving (Eq,Show)
data Article_Attrs = Article_Attrs
    { articleClass :: (Maybe Article_Class)
    , articleParentbook :: (Maybe String)
    , articleStatus :: (Maybe String)
    , articleId :: (Maybe String)
    , articleLang :: (Maybe String)
    , articleRevisionflag :: (Maybe Article_Revisionflag)
    , articleRole :: (Maybe String)
    } deriving (Eq,Show)
data Article_Class = Article_Class_Journalarticle  | 
		     Article_Class_Productsheet  | 
		     Article_Class_Whitepaper  |  Article_Class_Techreport
		      |  Article_Class_Specification  |  Article_Class_Faq
		   deriving (Eq,Show)
data Article_Revisionflag = Article_Revisionflag_Changed
			     |  Article_Revisionflag_Added  | 
			    Article_Revisionflag_Deleted  | 
			    Article_Revisionflag_Off
			  deriving (Eq,Show)
newtype Sectioninfo = Sectioninfo (List1 (OneOf32 Mediaobject Legalnotice Keywordset Subjectset Abbrev Abstract Author Authorgroup Bibliomisc Copyright Corpauthor Date Edition Editor Issuenum Othercredit Pubdate Publishername Releaseinfo Revhistory Subtitle Title Titleabbrev Volumenum Citetitle Honorific Firstname Surname Lineage Othername Affiliation Authorblurb)) 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Title where
    fromElem (CElem (Elem "title" as c0):rest) =
	(\(a,ca)->
	   (Just (Title (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Title as a) =
	[CElem (Elem "title" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Title_Attrs where
    fromAttrs as =
	Title_Attrs
	  { titlePagenum = possibleA fromAttrToStr "pagenum" as
	  , titleId = possibleA fromAttrToStr "id" as
	  , titleLang = possibleA fromAttrToStr "lang" as
	  , titleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , titleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "pagenum" (titlePagenum v)
	, maybeToAttr toAttrFrStr "id" (titleId v)
	, maybeToAttr toAttrFrStr "lang" (titleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (titleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (titleRole v)
	]
instance XmlContent Title_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Title_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Title_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Title_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Title_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Title_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Title_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Title_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Title_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Title_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Title_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Title_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Title_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Title_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Title_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Title_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Title_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Title_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Title_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Title_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Title_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Title_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Title_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Title_Author a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Title_Corpauthor a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Title_Othercredit a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Title_Revhistory a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Title_Inlinemediaobject a), rest)
																											(_,_) ->
																											    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Title_Str a) = toText a
    toElem (Title_Footnoteref a) = toElem a
    toElem (Title_Xref a) = toElem a
    toElem (Title_Abbrev a) = toElem a
    toElem (Title_Acronym a) = toElem a
    toElem (Title_Citetitle a) = toElem a
    toElem (Title_Emphasis a) = toElem a
    toElem (Title_Footnote a) = toElem a
    toElem (Title_Phrase a) = toElem a
    toElem (Title_Quote a) = toElem a
    toElem (Title_Trademark a) = toElem a
    toElem (Title_Link a) = toElem a
    toElem (Title_Ulink a) = toElem a
    toElem (Title_Command a) = toElem a
    toElem (Title_Computeroutput a) = toElem a
    toElem (Title_Email a) = toElem a
    toElem (Title_Filename a) = toElem a
    toElem (Title_Literal a) = toElem a
    toElem (Title_Option a) = toElem a
    toElem (Title_Replaceable a) = toElem a
    toElem (Title_Systemitem a) = toElem a
    toElem (Title_Userinput a) = toElem a
    toElem (Title_Author a) = toElem a
    toElem (Title_Corpauthor a) = toElem a
    toElem (Title_Othercredit a) = toElem a
    toElem (Title_Revhistory a) = toElem a
    toElem (Title_Inlinemediaobject a) = toElem a
instance XmlAttrType Title_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Title_Revisionflag_Changed
	    translate "added" = Just Title_Revisionflag_Added
	    translate "deleted" = Just Title_Revisionflag_Deleted
	    translate "off" = Just Title_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Title_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Title_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Title_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Title_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Titleabbrev where
    fromElem (CElem (Elem "titleabbrev" as c0):rest) =
	(\(a,ca)->
	   (Just (Titleabbrev (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Titleabbrev as a) =
	[CElem (Elem "titleabbrev" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Titleabbrev_Attrs where
    fromAttrs as =
	Titleabbrev_Attrs
	  { titleabbrevId = possibleA fromAttrToStr "id" as
	  , titleabbrevLang = possibleA fromAttrToStr "lang" as
	  , titleabbrevRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , titleabbrevRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (titleabbrevId v)
	, maybeToAttr toAttrFrStr "lang" (titleabbrevLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (titleabbrevRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (titleabbrevRole v)
	]
instance XmlContent Titleabbrev_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Titleabbrev_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Titleabbrev_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Titleabbrev_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Titleabbrev_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Titleabbrev_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Titleabbrev_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Titleabbrev_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Titleabbrev_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Titleabbrev_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Titleabbrev_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Titleabbrev_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Titleabbrev_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Titleabbrev_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Titleabbrev_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Titleabbrev_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Titleabbrev_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Titleabbrev_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Titleabbrev_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Titleabbrev_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Titleabbrev_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Titleabbrev_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Titleabbrev_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Titleabbrev_Author a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Titleabbrev_Corpauthor a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Titleabbrev_Othercredit a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Titleabbrev_Revhistory a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Titleabbrev_Inlinemediaobject a), rest)
																											(_,_) ->
																											    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Titleabbrev_Str a) = toText a
    toElem (Titleabbrev_Footnoteref a) = toElem a
    toElem (Titleabbrev_Xref a) = toElem a
    toElem (Titleabbrev_Abbrev a) = toElem a
    toElem (Titleabbrev_Acronym a) = toElem a
    toElem (Titleabbrev_Citetitle a) = toElem a
    toElem (Titleabbrev_Emphasis a) = toElem a
    toElem (Titleabbrev_Footnote a) = toElem a
    toElem (Titleabbrev_Phrase a) = toElem a
    toElem (Titleabbrev_Quote a) = toElem a
    toElem (Titleabbrev_Trademark a) = toElem a
    toElem (Titleabbrev_Link a) = toElem a
    toElem (Titleabbrev_Ulink a) = toElem a
    toElem (Titleabbrev_Command a) = toElem a
    toElem (Titleabbrev_Computeroutput a) = toElem a
    toElem (Titleabbrev_Email a) = toElem a
    toElem (Titleabbrev_Filename a) = toElem a
    toElem (Titleabbrev_Literal a) = toElem a
    toElem (Titleabbrev_Option a) = toElem a
    toElem (Titleabbrev_Replaceable a) = toElem a
    toElem (Titleabbrev_Systemitem a) = toElem a
    toElem (Titleabbrev_Userinput a) = toElem a
    toElem (Titleabbrev_Author a) = toElem a
    toElem (Titleabbrev_Corpauthor a) = toElem a
    toElem (Titleabbrev_Othercredit a) = toElem a
    toElem (Titleabbrev_Revhistory a) = toElem a
    toElem (Titleabbrev_Inlinemediaobject a) = toElem a
instance XmlAttrType Titleabbrev_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Titleabbrev_Revisionflag_Changed
	    translate "added" = Just Titleabbrev_Revisionflag_Added
	    translate "deleted" = Just Titleabbrev_Revisionflag_Deleted
	    translate "off" = Just Titleabbrev_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Titleabbrev_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Titleabbrev_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Titleabbrev_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Titleabbrev_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Subtitle where
    fromElem (CElem (Elem "subtitle" as c0):rest) =
	(\(a,ca)->
	   (Just (Subtitle (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subtitle as a) =
	[CElem (Elem "subtitle" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Subtitle_Attrs where
    fromAttrs as =
	Subtitle_Attrs
	  { subtitleId = possibleA fromAttrToStr "id" as
	  , subtitleLang = possibleA fromAttrToStr "lang" as
	  , subtitleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , subtitleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (subtitleId v)
	, maybeToAttr toAttrFrStr "lang" (subtitleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (subtitleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (subtitleRole v)
	]
instance XmlContent Subtitle_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Subtitle_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Subtitle_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Subtitle_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Subtitle_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Subtitle_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Subtitle_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Subtitle_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Subtitle_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Subtitle_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Subtitle_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Subtitle_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Subtitle_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Subtitle_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Subtitle_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Subtitle_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Subtitle_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Subtitle_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Subtitle_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Subtitle_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Subtitle_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Subtitle_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Subtitle_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Subtitle_Author a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Subtitle_Corpauthor a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Subtitle_Othercredit a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Subtitle_Revhistory a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Subtitle_Inlinemediaobject a), rest)
																											(_,_) ->
																											    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subtitle_Str a) = toText a
    toElem (Subtitle_Footnoteref a) = toElem a
    toElem (Subtitle_Xref a) = toElem a
    toElem (Subtitle_Abbrev a) = toElem a
    toElem (Subtitle_Acronym a) = toElem a
    toElem (Subtitle_Citetitle a) = toElem a
    toElem (Subtitle_Emphasis a) = toElem a
    toElem (Subtitle_Footnote a) = toElem a
    toElem (Subtitle_Phrase a) = toElem a
    toElem (Subtitle_Quote a) = toElem a
    toElem (Subtitle_Trademark a) = toElem a
    toElem (Subtitle_Link a) = toElem a
    toElem (Subtitle_Ulink a) = toElem a
    toElem (Subtitle_Command a) = toElem a
    toElem (Subtitle_Computeroutput a) = toElem a
    toElem (Subtitle_Email a) = toElem a
    toElem (Subtitle_Filename a) = toElem a
    toElem (Subtitle_Literal a) = toElem a
    toElem (Subtitle_Option a) = toElem a
    toElem (Subtitle_Replaceable a) = toElem a
    toElem (Subtitle_Systemitem a) = toElem a
    toElem (Subtitle_Userinput a) = toElem a
    toElem (Subtitle_Author a) = toElem a
    toElem (Subtitle_Corpauthor a) = toElem a
    toElem (Subtitle_Othercredit a) = toElem a
    toElem (Subtitle_Revhistory a) = toElem a
    toElem (Subtitle_Inlinemediaobject a) = toElem a
instance XmlAttrType Subtitle_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Subtitle_Revisionflag_Changed
	    translate "added" = Just Subtitle_Revisionflag_Added
	    translate "deleted" = Just Subtitle_Revisionflag_Deleted
	    translate "off" = Just Subtitle_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Subtitle_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Subtitle_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Subtitle_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Subtitle_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Bibliomixed where
    fromElem (CElem (Elem "bibliomixed" as c0):rest) =
	(\(a,ca)->
	   (Just (Bibliomixed (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomixed as a) =
	[CElem (Elem "bibliomixed" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Bibliomixed_Attrs where
    fromAttrs as =
	Bibliomixed_Attrs
	  { bibliomixedId = possibleA fromAttrToStr "id" as
	  , bibliomixedLang = possibleA fromAttrToStr "lang" as
	  , bibliomixedRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , bibliomixedRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (bibliomixedId v)
	, maybeToAttr toAttrFrStr "lang" (bibliomixedLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (bibliomixedRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (bibliomixedRole v)
	]
instance XmlContent Bibliomixed_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Bibliomixed_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Bibliomixed_Abbrev a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Bibliomixed_Abstract a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Bibliomixed_Author a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Bibliomixed_Authorgroup a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Bibliomixed_Bibliomisc a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Bibliomixed_Copyright a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Bibliomixed_Corpauthor a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Bibliomixed_Date a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Bibliomixed_Edition a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Bibliomixed_Editor a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Bibliomixed_Issuenum a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Bibliomixed_Othercredit a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Bibliomixed_Pubdate a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Bibliomixed_Publishername a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Bibliomixed_Releaseinfo a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Bibliomixed_Revhistory a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Bibliomixed_Subtitle a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Bibliomixed_Title a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Bibliomixed_Titleabbrev a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Bibliomixed_Volumenum a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Bibliomixed_Citetitle a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Bibliomixed_Honorific a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Bibliomixed_Firstname a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Bibliomixed_Surname a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Bibliomixed_Lineage a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Bibliomixed_Othername a), rest)
																											(_,_) ->
																												case (fromElem c0) of
																												(Just a,rest) -> (Just (Bibliomixed_Affiliation a), rest)
																												(_,_) ->
																													case (fromElem c0) of
																													(Just a,rest) -> (Just (Bibliomixed_Authorblurb a), rest)
																													(_,_) ->
																														case (fromElem c0) of
																														(Just a,rest) -> (Just (Bibliomixed_Bibliomset a), rest)
																														(_,_) ->
																														    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomixed_Str a) = toText a
    toElem (Bibliomixed_Abbrev a) = toElem a
    toElem (Bibliomixed_Abstract a) = toElem a
    toElem (Bibliomixed_Author a) = toElem a
    toElem (Bibliomixed_Authorgroup a) = toElem a
    toElem (Bibliomixed_Bibliomisc a) = toElem a
    toElem (Bibliomixed_Copyright a) = toElem a
    toElem (Bibliomixed_Corpauthor a) = toElem a
    toElem (Bibliomixed_Date a) = toElem a
    toElem (Bibliomixed_Edition a) = toElem a
    toElem (Bibliomixed_Editor a) = toElem a
    toElem (Bibliomixed_Issuenum a) = toElem a
    toElem (Bibliomixed_Othercredit a) = toElem a
    toElem (Bibliomixed_Pubdate a) = toElem a
    toElem (Bibliomixed_Publishername a) = toElem a
    toElem (Bibliomixed_Releaseinfo a) = toElem a
    toElem (Bibliomixed_Revhistory a) = toElem a
    toElem (Bibliomixed_Subtitle a) = toElem a
    toElem (Bibliomixed_Title a) = toElem a
    toElem (Bibliomixed_Titleabbrev a) = toElem a
    toElem (Bibliomixed_Volumenum a) = toElem a
    toElem (Bibliomixed_Citetitle a) = toElem a
    toElem (Bibliomixed_Honorific a) = toElem a
    toElem (Bibliomixed_Firstname a) = toElem a
    toElem (Bibliomixed_Surname a) = toElem a
    toElem (Bibliomixed_Lineage a) = toElem a
    toElem (Bibliomixed_Othername a) = toElem a
    toElem (Bibliomixed_Affiliation a) = toElem a
    toElem (Bibliomixed_Authorblurb a) = toElem a
    toElem (Bibliomixed_Bibliomset a) = toElem a
instance XmlAttrType Bibliomixed_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Bibliomixed_Revisionflag_Changed
	    translate "added" = Just Bibliomixed_Revisionflag_Added
	    translate "deleted" = Just Bibliomixed_Revisionflag_Deleted
	    translate "off" = Just Bibliomixed_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Bibliomixed_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Bibliomixed_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Bibliomixed_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Bibliomixed_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Articleinfo where
    fromElem (CElem (Elem "articleinfo" as c0):rest) =
	(\(a,ca)->
	   (Just (Articleinfo (fromAttrs as) a), rest))
	(definite fromElem "(mediaobject|legalnotice|subjectset|keywordset|abbrev|abstract|author|authorgroup|bibliomisc|copyright|corpauthor|date|edition|editor|issuenum|othercredit|pubdate|publishername|releaseinfo|revhistory|subtitle|title|titleabbrev|volumenum|citetitle|honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "articleinfo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Articleinfo as a) =
	[CElem (Elem "articleinfo" (toAttrs as) (toElem a))]
instance XmlAttributes Articleinfo_Attrs where
    fromAttrs as =
	Articleinfo_Attrs
	  { articleinfoId = possibleA fromAttrToStr "id" as
	  , articleinfoLang = possibleA fromAttrToStr "lang" as
	  , articleinfoRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , articleinfoRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (articleinfoId v)
	, maybeToAttr toAttrFrStr "lang" (articleinfoLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (articleinfoRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (articleinfoRole v)
	]
instance XmlAttrType Articleinfo_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Articleinfo_Revisionflag_Changed
	    translate "added" = Just Articleinfo_Revisionflag_Added
	    translate "deleted" = Just Articleinfo_Revisionflag_Deleted
	    translate "off" = Just Articleinfo_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Articleinfo_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Articleinfo_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Articleinfo_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Articleinfo_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Bibliomset where
    fromElem (CElem (Elem "bibliomset" as c0):rest) =
	(\(a,ca)->
	   (Just (Bibliomset (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomset as a) =
	[CElem (Elem "bibliomset" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Bibliomset_Attrs where
    fromAttrs as =
	Bibliomset_Attrs
	  { bibliomsetRelation = possibleA fromAttrToStr "relation" as
	  , bibliomsetId = possibleA fromAttrToStr "id" as
	  , bibliomsetLang = possibleA fromAttrToStr "lang" as
	  , bibliomsetRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , bibliomsetRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "relation" (bibliomsetRelation v)
	, maybeToAttr toAttrFrStr "id" (bibliomsetId v)
	, maybeToAttr toAttrFrStr "lang" (bibliomsetLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (bibliomsetRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (bibliomsetRole v)
	]
instance XmlContent Bibliomset_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Bibliomset_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Bibliomset_Abbrev a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Bibliomset_Abstract a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Bibliomset_Author a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Bibliomset_Authorgroup a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Bibliomset_Bibliomisc a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Bibliomset_Copyright a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Bibliomset_Corpauthor a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Bibliomset_Date a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Bibliomset_Edition a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Bibliomset_Editor a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Bibliomset_Issuenum a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Bibliomset_Othercredit a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Bibliomset_Pubdate a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Bibliomset_Publishername a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Bibliomset_Releaseinfo a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Bibliomset_Revhistory a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Bibliomset_Subtitle a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Bibliomset_Title a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Bibliomset_Titleabbrev a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Bibliomset_Volumenum a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Bibliomset_Citetitle a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Bibliomset_Honorific a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Bibliomset_Firstname a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Bibliomset_Surname a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Bibliomset_Lineage a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Bibliomset_Othername a), rest)
																											(_,_) ->
																												case (fromElem c0) of
																												(Just a,rest) -> (Just (Bibliomset_Affiliation a), rest)
																												(_,_) ->
																													case (fromElem c0) of
																													(Just a,rest) -> (Just (Bibliomset_Authorblurb a), rest)
																													(_,_) ->
																														case (fromElem c0) of
																														(Just a,rest) -> (Just (Bibliomset_Bibliomset a), rest)
																														(_,_) ->
																														    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomset_Str a) = toText a
    toElem (Bibliomset_Abbrev a) = toElem a
    toElem (Bibliomset_Abstract a) = toElem a
    toElem (Bibliomset_Author a) = toElem a
    toElem (Bibliomset_Authorgroup a) = toElem a
    toElem (Bibliomset_Bibliomisc a) = toElem a
    toElem (Bibliomset_Copyright a) = toElem a
    toElem (Bibliomset_Corpauthor a) = toElem a
    toElem (Bibliomset_Date a) = toElem a
    toElem (Bibliomset_Edition a) = toElem a
    toElem (Bibliomset_Editor a) = toElem a
    toElem (Bibliomset_Issuenum a) = toElem a
    toElem (Bibliomset_Othercredit a) = toElem a
    toElem (Bibliomset_Pubdate a) = toElem a
    toElem (Bibliomset_Publishername a) = toElem a
    toElem (Bibliomset_Releaseinfo a) = toElem a
    toElem (Bibliomset_Revhistory a) = toElem a
    toElem (Bibliomset_Subtitle a) = toElem a
    toElem (Bibliomset_Title a) = toElem a
    toElem (Bibliomset_Titleabbrev a) = toElem a
    toElem (Bibliomset_Volumenum a) = toElem a
    toElem (Bibliomset_Citetitle a) = toElem a
    toElem (Bibliomset_Honorific a) = toElem a
    toElem (Bibliomset_Firstname a) = toElem a
    toElem (Bibliomset_Surname a) = toElem a
    toElem (Bibliomset_Lineage a) = toElem a
    toElem (Bibliomset_Othername a) = toElem a
    toElem (Bibliomset_Affiliation a) = toElem a
    toElem (Bibliomset_Authorblurb a) = toElem a
    toElem (Bibliomset_Bibliomset a) = toElem a
instance XmlAttrType Bibliomset_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Bibliomset_Revisionflag_Changed
	    translate "added" = Just Bibliomset_Revisionflag_Added
	    translate "deleted" = Just Bibliomset_Revisionflag_Deleted
	    translate "off" = Just Bibliomset_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Bibliomset_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Bibliomset_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Bibliomset_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Bibliomset_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Bibliomisc where
    fromElem (CElem (Elem "bibliomisc" as c0):rest) =
	(\(a,ca)->
	   (Just (Bibliomisc (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomisc as a) =
	[CElem (Elem "bibliomisc" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Bibliomisc_Attrs where
    fromAttrs as =
	Bibliomisc_Attrs
	  { bibliomiscId = possibleA fromAttrToStr "id" as
	  , bibliomiscLang = possibleA fromAttrToStr "lang" as
	  , bibliomiscRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , bibliomiscRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (bibliomiscId v)
	, maybeToAttr toAttrFrStr "lang" (bibliomiscLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (bibliomiscRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (bibliomiscRole v)
	]
instance XmlContent Bibliomisc_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Bibliomisc_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Bibliomisc_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Bibliomisc_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Bibliomisc_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Bibliomisc_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Bibliomisc_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Bibliomisc_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Bibliomisc_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Bibliomisc_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Bibliomisc_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Bibliomisc_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Bibliomisc_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Bibliomisc_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Bibliomisc_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Bibliomisc_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Bibliomisc_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Bibliomisc_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Bibliomisc_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Bibliomisc_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Bibliomisc_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Bibliomisc_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Bibliomisc_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Bibliomisc_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliomisc_Str a) = toText a
    toElem (Bibliomisc_Footnoteref a) = toElem a
    toElem (Bibliomisc_Xref a) = toElem a
    toElem (Bibliomisc_Abbrev a) = toElem a
    toElem (Bibliomisc_Acronym a) = toElem a
    toElem (Bibliomisc_Citetitle a) = toElem a
    toElem (Bibliomisc_Emphasis a) = toElem a
    toElem (Bibliomisc_Footnote a) = toElem a
    toElem (Bibliomisc_Phrase a) = toElem a
    toElem (Bibliomisc_Quote a) = toElem a
    toElem (Bibliomisc_Trademark a) = toElem a
    toElem (Bibliomisc_Link a) = toElem a
    toElem (Bibliomisc_Ulink a) = toElem a
    toElem (Bibliomisc_Command a) = toElem a
    toElem (Bibliomisc_Computeroutput a) = toElem a
    toElem (Bibliomisc_Email a) = toElem a
    toElem (Bibliomisc_Filename a) = toElem a
    toElem (Bibliomisc_Literal a) = toElem a
    toElem (Bibliomisc_Option a) = toElem a
    toElem (Bibliomisc_Replaceable a) = toElem a
    toElem (Bibliomisc_Systemitem a) = toElem a
    toElem (Bibliomisc_Userinput a) = toElem a
    toElem (Bibliomisc_Inlinemediaobject a) = toElem a
instance XmlAttrType Bibliomisc_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Bibliomisc_Revisionflag_Changed
	    translate "added" = Just Bibliomisc_Revisionflag_Added
	    translate "deleted" = Just Bibliomisc_Revisionflag_Deleted
	    translate "off" = Just Bibliomisc_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Bibliomisc_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Bibliomisc_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Bibliomisc_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Bibliomisc_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Subjectset where
    fromElem (CElem (Elem "subjectset" as c0):rest) =
	(\(a,ca)->
	   (Just (Subjectset (fromAttrs as) a), rest))
	(definite fromElem "subject+" "subjectset" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subjectset as a) =
	[CElem (Elem "subjectset" (toAttrs as) (toElem a))]
instance XmlAttributes Subjectset_Attrs where
    fromAttrs as =
	Subjectset_Attrs
	  { subjectsetScheme = possibleA fromAttrToStr "scheme" as
	  , subjectsetId = possibleA fromAttrToStr "id" as
	  , subjectsetLang = possibleA fromAttrToStr "lang" as
	  , subjectsetRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , subjectsetRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "scheme" (subjectsetScheme v)
	, maybeToAttr toAttrFrStr "id" (subjectsetId v)
	, maybeToAttr toAttrFrStr "lang" (subjectsetLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (subjectsetRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (subjectsetRole v)
	]
instance XmlAttrType Subjectset_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Subjectset_Revisionflag_Changed
	    translate "added" = Just Subjectset_Revisionflag_Added
	    translate "deleted" = Just Subjectset_Revisionflag_Deleted
	    translate "off" = Just Subjectset_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Subjectset_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Subjectset_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Subjectset_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Subjectset_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Subject where
    fromElem (CElem (Elem "subject" as c0):rest) =
	(\(a,ca)->
	   (Just (Subject (fromAttrs as) a), rest))
	(definite fromElem "subjectterm+" "subject" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subject as a) =
	[CElem (Elem "subject" (toAttrs as) (toElem a))]
instance XmlAttributes Subject_Attrs where
    fromAttrs as =
	Subject_Attrs
	  { subjectWeight = possibleA fromAttrToStr "weight" as
	  , subjectId = possibleA fromAttrToStr "id" as
	  , subjectLang = possibleA fromAttrToStr "lang" as
	  , subjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , subjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "weight" (subjectWeight v)
	, maybeToAttr toAttrFrStr "id" (subjectId v)
	, maybeToAttr toAttrFrStr "lang" (subjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (subjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (subjectRole v)
	]
instance XmlAttrType Subject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Subject_Revisionflag_Changed
	    translate "added" = Just Subject_Revisionflag_Added
	    translate "deleted" = Just Subject_Revisionflag_Deleted
	    translate "off" = Just Subject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Subject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Subject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Subject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Subject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Subjectterm where
    fromElem (CElem (Elem "subjectterm" as c0):rest) =
	(\(a,ca)->
	   (Just (Subjectterm (fromAttrs as) a), rest))
	(definite fromText "text" "subjectterm" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subjectterm as a) =
	[CElem (Elem "subjectterm" (toAttrs as) (toText a))]
instance XmlAttributes Subjectterm_Attrs where
    fromAttrs as =
	Subjectterm_Attrs
	  { subjecttermId = possibleA fromAttrToStr "id" as
	  , subjecttermLang = possibleA fromAttrToStr "lang" as
	  , subjecttermRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , subjecttermRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (subjecttermId v)
	, maybeToAttr toAttrFrStr "lang" (subjecttermLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (subjecttermRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (subjecttermRole v)
	]
instance XmlAttrType Subjectterm_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Subjectterm_Revisionflag_Changed
	    translate "added" = Just Subjectterm_Revisionflag_Added
	    translate "deleted" = Just Subjectterm_Revisionflag_Deleted
	    translate "off" = Just Subjectterm_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Subjectterm_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Subjectterm_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Subjectterm_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Subjectterm_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Keywordset where
    fromElem (CElem (Elem "keywordset" as c0):rest) =
	(\(a,ca)->
	   (Just (Keywordset (fromAttrs as) a), rest))
	(definite fromElem "keyword+" "keywordset" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Keywordset as a) =
	[CElem (Elem "keywordset" (toAttrs as) (toElem a))]
instance XmlAttributes Keywordset_Attrs where
    fromAttrs as =
	Keywordset_Attrs
	  { keywordsetId = possibleA fromAttrToStr "id" as
	  , keywordsetLang = possibleA fromAttrToStr "lang" as
	  , keywordsetRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , keywordsetRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (keywordsetId v)
	, maybeToAttr toAttrFrStr "lang" (keywordsetLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (keywordsetRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (keywordsetRole v)
	]
instance XmlAttrType Keywordset_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Keywordset_Revisionflag_Changed
	    translate "added" = Just Keywordset_Revisionflag_Added
	    translate "deleted" = Just Keywordset_Revisionflag_Deleted
	    translate "off" = Just Keywordset_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Keywordset_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Keywordset_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Keywordset_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Keywordset_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Keyword where
    fromElem (CElem (Elem "keyword" as c0):rest) =
	(\(a,ca)->
	   (Just (Keyword (fromAttrs as) a), rest))
	(definite fromText "text" "keyword" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Keyword as a) =
	[CElem (Elem "keyword" (toAttrs as) (toText a))]
instance XmlAttributes Keyword_Attrs where
    fromAttrs as =
	Keyword_Attrs
	  { keywordId = possibleA fromAttrToStr "id" as
	  , keywordLang = possibleA fromAttrToStr "lang" as
	  , keywordRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , keywordRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (keywordId v)
	, maybeToAttr toAttrFrStr "lang" (keywordLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (keywordRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (keywordRole v)
	]
instance XmlAttrType Keyword_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Keyword_Revisionflag_Changed
	    translate "added" = Just Keyword_Revisionflag_Added
	    translate "deleted" = Just Keyword_Revisionflag_Deleted
	    translate "off" = Just Keyword_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Keyword_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Keyword_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Keyword_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Keyword_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Sidebar where
    fromElem (CElem (Elem "sidebar" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Sidebar (fromAttrs as) a b), rest))
	   (definite fromElem "(itemizedlist|orderedlist|variablelist|note|literallayout|programlisting|para|blockquote|mediaobject|informaltable|example|figure|table)+" "sidebar" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Sidebar as a b) =
	[CElem (Elem "sidebar" (toAttrs as) (maybe [] toElem a
					     ++ toElem b))]
instance XmlAttributes Sidebar_Attrs where
    fromAttrs as =
	Sidebar_Attrs
	  { sidebarId = possibleA fromAttrToStr "id" as
	  , sidebarLang = possibleA fromAttrToStr "lang" as
	  , sidebarRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , sidebarRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (sidebarId v)
	, maybeToAttr toAttrFrStr "lang" (sidebarLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (sidebarRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (sidebarRole v)
	]
instance XmlAttrType Sidebar_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Sidebar_Revisionflag_Changed
	    translate "added" = Just Sidebar_Revisionflag_Added
	    translate "deleted" = Just Sidebar_Revisionflag_Deleted
	    translate "off" = Just Sidebar_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Sidebar_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Sidebar_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Sidebar_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Sidebar_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Abstract where
    fromElem (CElem (Elem "abstract" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Abstract (fromAttrs as) a b), rest))
	   (definite fromElem "(para)+" "abstract" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Abstract as a b) =
	[CElem (Elem "abstract" (toAttrs as) (maybe [] toElem a
					      ++ toElem b))]
instance XmlAttributes Abstract_Attrs where
    fromAttrs as =
	Abstract_Attrs
	  { abstractId = possibleA fromAttrToStr "id" as
	  , abstractLang = possibleA fromAttrToStr "lang" as
	  , abstractRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , abstractRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (abstractId v)
	, maybeToAttr toAttrFrStr "lang" (abstractLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (abstractRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (abstractRole v)
	]
instance XmlAttrType Abstract_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Abstract_Revisionflag_Changed
	    translate "added" = Just Abstract_Revisionflag_Added
	    translate "deleted" = Just Abstract_Revisionflag_Deleted
	    translate "off" = Just Abstract_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Abstract_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Abstract_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Abstract_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Abstract_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Authorblurb where
    fromElem (CElem (Elem "authorblurb" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Authorblurb (fromAttrs as) a b), rest))
	   (definite fromElem "(para)+" "authorblurb" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Authorblurb as a b) =
	[CElem (Elem "authorblurb" (toAttrs as) (maybe [] toElem a
						 ++ toElem b))]
instance XmlAttributes Authorblurb_Attrs where
    fromAttrs as =
	Authorblurb_Attrs
	  { authorblurbId = possibleA fromAttrToStr "id" as
	  , authorblurbLang = possibleA fromAttrToStr "lang" as
	  , authorblurbRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , authorblurbRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (authorblurbId v)
	, maybeToAttr toAttrFrStr "lang" (authorblurbLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (authorblurbRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (authorblurbRole v)
	]
instance XmlAttrType Authorblurb_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Authorblurb_Revisionflag_Changed
	    translate "added" = Just Authorblurb_Revisionflag_Added
	    translate "deleted" = Just Authorblurb_Revisionflag_Deleted
	    translate "off" = Just Authorblurb_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Authorblurb_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Authorblurb_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Authorblurb_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Authorblurb_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Blockquote where
    fromElem (CElem (Elem "blockquote" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Blockquote (fromAttrs as) a b c), rest))
	      (definite fromElem "(itemizedlist|orderedlist|variablelist|note|literallayout|programlisting|para|blockquote|mediaobject|informaltable|example|figure|table|sidebar|abstract|authorblurb|epigraph)+" "blockquote" cb))
	   (fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Blockquote as a b c) =
	[CElem (Elem "blockquote" (toAttrs as) (maybe [] toElem a
						++ maybe [] toElem b ++
						toElem c))]
instance XmlAttributes Blockquote_Attrs where
    fromAttrs as =
	Blockquote_Attrs
	  { blockquoteId = possibleA fromAttrToStr "id" as
	  , blockquoteLang = possibleA fromAttrToStr "lang" as
	  , blockquoteRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , blockquoteRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (blockquoteId v)
	, maybeToAttr toAttrFrStr "lang" (blockquoteLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (blockquoteRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (blockquoteRole v)
	]
instance XmlAttrType Blockquote_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Blockquote_Revisionflag_Changed
	    translate "added" = Just Blockquote_Revisionflag_Added
	    translate "deleted" = Just Blockquote_Revisionflag_Deleted
	    translate "off" = Just Blockquote_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Blockquote_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Blockquote_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Blockquote_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Blockquote_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Attribution where
    fromElem (CElem (Elem "attribution" as c0):rest) =
	(\(a,ca)->
	   (Just (Attribution (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Attribution as a) =
	[CElem (Elem "attribution" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Attribution_Attrs where
    fromAttrs as =
	Attribution_Attrs
	  { attributionId = possibleA fromAttrToStr "id" as
	  , attributionLang = possibleA fromAttrToStr "lang" as
	  , attributionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , attributionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (attributionId v)
	, maybeToAttr toAttrFrStr "lang" (attributionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (attributionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (attributionRole v)
	]
instance XmlContent Attribution_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Attribution_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Attribution_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Attribution_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Attribution_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Attribution_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Attribution_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Attribution_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Attribution_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Attribution_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Attribution_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Attribution_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Attribution_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Attribution_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Attribution_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Attribution_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Attribution_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Attribution_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Attribution_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Attribution_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Attribution_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Attribution_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Attribution_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Attribution_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Attribution_Str a) = toText a
    toElem (Attribution_Footnoteref a) = toElem a
    toElem (Attribution_Xref a) = toElem a
    toElem (Attribution_Abbrev a) = toElem a
    toElem (Attribution_Acronym a) = toElem a
    toElem (Attribution_Citetitle a) = toElem a
    toElem (Attribution_Emphasis a) = toElem a
    toElem (Attribution_Footnote a) = toElem a
    toElem (Attribution_Phrase a) = toElem a
    toElem (Attribution_Quote a) = toElem a
    toElem (Attribution_Trademark a) = toElem a
    toElem (Attribution_Link a) = toElem a
    toElem (Attribution_Ulink a) = toElem a
    toElem (Attribution_Command a) = toElem a
    toElem (Attribution_Computeroutput a) = toElem a
    toElem (Attribution_Email a) = toElem a
    toElem (Attribution_Filename a) = toElem a
    toElem (Attribution_Literal a) = toElem a
    toElem (Attribution_Option a) = toElem a
    toElem (Attribution_Replaceable a) = toElem a
    toElem (Attribution_Systemitem a) = toElem a
    toElem (Attribution_Userinput a) = toElem a
    toElem (Attribution_Inlinemediaobject a) = toElem a
instance XmlAttrType Attribution_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Attribution_Revisionflag_Changed
	    translate "added" = Just Attribution_Revisionflag_Added
	    translate "deleted" = Just Attribution_Revisionflag_Deleted
	    translate "off" = Just Attribution_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Attribution_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Attribution_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Attribution_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Attribution_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Epigraph where
    fromElem (CElem (Elem "epigraph" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Epigraph (fromAttrs as) a b), rest))
	   (definite fromElem "(para|literallayout)+" "epigraph" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Epigraph as a b) =
	[CElem (Elem "epigraph" (toAttrs as) (maybe [] toElem a
					      ++ toElem b))]
instance XmlAttributes Epigraph_Attrs where
    fromAttrs as =
	Epigraph_Attrs
	  { epigraphId = possibleA fromAttrToStr "id" as
	  , epigraphLang = possibleA fromAttrToStr "lang" as
	  , epigraphRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , epigraphRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (epigraphId v)
	, maybeToAttr toAttrFrStr "lang" (epigraphLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (epigraphRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (epigraphRole v)
	]
instance XmlAttrType Epigraph_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Epigraph_Revisionflag_Changed
	    translate "added" = Just Epigraph_Revisionflag_Added
	    translate "deleted" = Just Epigraph_Revisionflag_Deleted
	    translate "off" = Just Epigraph_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Epigraph_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Epigraph_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Epigraph_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Epigraph_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Footnote where
    fromElem (CElem (Elem "footnote" as c0):rest) =
	(\(a,ca)->
	   (Just (Footnote (fromAttrs as) a), rest))
	(definite fromElem "(itemizedlist|orderedlist|variablelist|literallayout|programlisting|para|blockquote|mediaobject|informaltable)+" "footnote" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Footnote as a) =
	[CElem (Elem "footnote" (toAttrs as) (toElem a))]
instance XmlAttributes Footnote_Attrs where
    fromAttrs as =
	Footnote_Attrs
	  { footnoteLabel = possibleA fromAttrToStr "label" as
	  , footnoteId = possibleA fromAttrToStr "id" as
	  , footnoteLang = possibleA fromAttrToStr "lang" as
	  , footnoteRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , footnoteRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (footnoteLabel v)
	, maybeToAttr toAttrFrStr "id" (footnoteId v)
	, maybeToAttr toAttrFrStr "lang" (footnoteLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (footnoteRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (footnoteRole v)
	]
instance XmlAttrType Footnote_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Footnote_Revisionflag_Changed
	    translate "added" = Just Footnote_Revisionflag_Added
	    translate "deleted" = Just Footnote_Revisionflag_Deleted
	    translate "off" = Just Footnote_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Footnote_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Footnote_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Footnote_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Footnote_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Para where
    fromElem (CElem (Elem "para" as c0):rest) =
	(\(a,ca)->
	   (Just (Para (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Para as a) =
	[CElem (Elem "para" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Para_Attrs where
    fromAttrs as =
	Para_Attrs
	  { paraId = possibleA fromAttrToStr "id" as
	  , paraLang = possibleA fromAttrToStr "lang" as
	  , paraRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , paraRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (paraId v)
	, maybeToAttr toAttrFrStr "lang" (paraLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (paraRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (paraRole v)
	]
instance XmlContent Para_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Para_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Para_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Para_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Para_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Para_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Para_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Para_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Para_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Para_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Para_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Para_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Para_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Para_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Para_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Para_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Para_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Para_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Para_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Para_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Para_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Para_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Para_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Para_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Para_Str a) = toText a
    toElem (Para_Footnoteref a) = toElem a
    toElem (Para_Xref a) = toElem a
    toElem (Para_Abbrev a) = toElem a
    toElem (Para_Acronym a) = toElem a
    toElem (Para_Citetitle a) = toElem a
    toElem (Para_Emphasis a) = toElem a
    toElem (Para_Footnote a) = toElem a
    toElem (Para_Phrase a) = toElem a
    toElem (Para_Quote a) = toElem a
    toElem (Para_Trademark a) = toElem a
    toElem (Para_Link a) = toElem a
    toElem (Para_Ulink a) = toElem a
    toElem (Para_Command a) = toElem a
    toElem (Para_Computeroutput a) = toElem a
    toElem (Para_Email a) = toElem a
    toElem (Para_Filename a) = toElem a
    toElem (Para_Literal a) = toElem a
    toElem (Para_Option a) = toElem a
    toElem (Para_Replaceable a) = toElem a
    toElem (Para_Systemitem a) = toElem a
    toElem (Para_Userinput a) = toElem a
    toElem (Para_Inlinemediaobject a) = toElem a
instance XmlAttrType Para_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Para_Revisionflag_Changed
	    translate "added" = Just Para_Revisionflag_Added
	    translate "deleted" = Just Para_Revisionflag_Deleted
	    translate "off" = Just Para_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Para_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Para_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Para_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Para_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Note where
    fromElem (CElem (Elem "note" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Note (fromAttrs as) a b), rest))
	   (definite fromElem "(itemizedlist|orderedlist|variablelist|literallayout|programlisting|para|blockquote|mediaobject|informaltable|example|figure|table)+" "note" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Note as a b) =
	[CElem (Elem "note" (toAttrs as) (maybe [] toElem a
					  ++ toElem b))]
instance XmlAttributes Note_Attrs where
    fromAttrs as =
	Note_Attrs
	  { noteId = possibleA fromAttrToStr "id" as
	  , noteLang = possibleA fromAttrToStr "lang" as
	  , noteRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , noteRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (noteId v)
	, maybeToAttr toAttrFrStr "lang" (noteLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (noteRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (noteRole v)
	]
instance XmlAttrType Note_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Note_Revisionflag_Changed
	    translate "added" = Just Note_Revisionflag_Added
	    translate "deleted" = Just Note_Revisionflag_Deleted
	    translate "off" = Just Note_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Note_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Note_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Note_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Note_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Itemizedlist where
    fromElem (CElem (Elem "itemizedlist" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Itemizedlist (fromAttrs as) a b c), rest))
	      (definite fromElem "listitem+" "itemizedlist" cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Itemizedlist as a b c) =
	[CElem (Elem "itemizedlist" (toAttrs as) (maybe [] toElem a
						  ++ concatMap toElem b ++
						  toElem c))]
instance XmlAttributes Itemizedlist_Attrs where
    fromAttrs as =
	Itemizedlist_Attrs
	  { itemizedlistSpacing = possibleA fromAttrToTyp "spacing" as
	  , itemizedlistMark = possibleA fromAttrToStr "mark" as
	  , itemizedlistId = possibleA fromAttrToStr "id" as
	  , itemizedlistLang = possibleA fromAttrToStr "lang" as
	  , itemizedlistRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , itemizedlistRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "spacing" (itemizedlistSpacing v)
	, maybeToAttr toAttrFrStr "mark" (itemizedlistMark v)
	, maybeToAttr toAttrFrStr "id" (itemizedlistId v)
	, maybeToAttr toAttrFrStr "lang" (itemizedlistLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (itemizedlistRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (itemizedlistRole v)
	]
instance XmlAttrType Itemizedlist_Spacing where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "normal" = Just Itemizedlist_Spacing_Normal
	    translate "compact" = Just Itemizedlist_Spacing_Compact
	    translate _ = Nothing
    toAttrFrTyp n Itemizedlist_Spacing_Normal = Just (n, str2attr "normal")
    toAttrFrTyp n Itemizedlist_Spacing_Compact = Just (n, str2attr "compact")
instance XmlAttrType Itemizedlist_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Itemizedlist_Revisionflag_Changed
	    translate "added" = Just Itemizedlist_Revisionflag_Added
	    translate "deleted" = Just Itemizedlist_Revisionflag_Deleted
	    translate "off" = Just Itemizedlist_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Itemizedlist_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Itemizedlist_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Itemizedlist_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Itemizedlist_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Orderedlist where
    fromElem (CElem (Elem "orderedlist" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Orderedlist (fromAttrs as) a b c), rest))
	      (definite fromElem "listitem+" "orderedlist" cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Orderedlist as a b c) =
	[CElem (Elem "orderedlist" (toAttrs as) (maybe [] toElem a
						 ++ concatMap toElem b ++
						 toElem c))]
instance XmlAttributes Orderedlist_Attrs where
    fromAttrs as =
	Orderedlist_Attrs
	  { orderedlistNumeration = possibleA fromAttrToTyp "numeration" as
	  , orderedlistInheritnum = defaultA fromAttrToTyp Orderedlist_Inheritnum_Ignore "inheritnum" as
	  , orderedlistContinuation = defaultA fromAttrToTyp Orderedlist_Continuation_Restarts "continuation" as
	  , orderedlistSpacing = possibleA fromAttrToTyp "spacing" as
	  , orderedlistId = possibleA fromAttrToStr "id" as
	  , orderedlistLang = possibleA fromAttrToStr "lang" as
	  , orderedlistRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , orderedlistRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "numeration" (orderedlistNumeration v)
	, defaultToAttr toAttrFrTyp "inheritnum" (orderedlistInheritnum v)
	, defaultToAttr toAttrFrTyp "continuation" (orderedlistContinuation v)
	, maybeToAttr toAttrFrTyp "spacing" (orderedlistSpacing v)
	, maybeToAttr toAttrFrStr "id" (orderedlistId v)
	, maybeToAttr toAttrFrStr "lang" (orderedlistLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (orderedlistRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (orderedlistRole v)
	]
instance XmlAttrType Orderedlist_Numeration where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "arabic" = Just Orderedlist_Numeration_Arabic
	    translate "upperalpha" = Just Orderedlist_Numeration_Upperalpha
	    translate "loweralpha" = Just Orderedlist_Numeration_Loweralpha
	    translate "upperroman" = Just Orderedlist_Numeration_Upperroman
	    translate "lowerroman" = Just Orderedlist_Numeration_Lowerroman
	    translate _ = Nothing
    toAttrFrTyp n Orderedlist_Numeration_Arabic = Just (n, str2attr "arabic")
    toAttrFrTyp n Orderedlist_Numeration_Upperalpha = Just (n, str2attr "upperalpha")
    toAttrFrTyp n Orderedlist_Numeration_Loweralpha = Just (n, str2attr "loweralpha")
    toAttrFrTyp n Orderedlist_Numeration_Upperroman = Just (n, str2attr "upperroman")
    toAttrFrTyp n Orderedlist_Numeration_Lowerroman = Just (n, str2attr "lowerroman")
instance XmlAttrType Orderedlist_Inheritnum where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "inherit" = Just Orderedlist_Inheritnum_Inherit
	    translate "ignore" = Just Orderedlist_Inheritnum_Ignore
	    translate _ = Nothing
    toAttrFrTyp n Orderedlist_Inheritnum_Inherit = Just (n, str2attr "inherit")
    toAttrFrTyp n Orderedlist_Inheritnum_Ignore = Just (n, str2attr "ignore")
instance XmlAttrType Orderedlist_Continuation where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "continues" = Just Orderedlist_Continuation_Continues
	    translate "restarts" = Just Orderedlist_Continuation_Restarts
	    translate _ = Nothing
    toAttrFrTyp n Orderedlist_Continuation_Continues = Just (n, str2attr "continues")
    toAttrFrTyp n Orderedlist_Continuation_Restarts = Just (n, str2attr "restarts")
instance XmlAttrType Orderedlist_Spacing where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "normal" = Just Orderedlist_Spacing_Normal
	    translate "compact" = Just Orderedlist_Spacing_Compact
	    translate _ = Nothing
    toAttrFrTyp n Orderedlist_Spacing_Normal = Just (n, str2attr "normal")
    toAttrFrTyp n Orderedlist_Spacing_Compact = Just (n, str2attr "compact")
instance XmlAttrType Orderedlist_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Orderedlist_Revisionflag_Changed
	    translate "added" = Just Orderedlist_Revisionflag_Added
	    translate "deleted" = Just Orderedlist_Revisionflag_Deleted
	    translate "off" = Just Orderedlist_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Orderedlist_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Orderedlist_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Orderedlist_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Orderedlist_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Listitem where
    fromElem (CElem (Elem "listitem" as c0):rest) =
	(\(a,ca)->
	   (Just (Listitem (fromAttrs as) a), rest))
	(definite fromElem "(itemizedlist|orderedlist|variablelist|note|literallayout|programlisting|para|blockquote|mediaobject|informaltable|example|figure|table|sidebar|abstract|authorblurb|epigraph)+" "listitem" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Listitem as a) =
	[CElem (Elem "listitem" (toAttrs as) (toElem a))]
instance XmlAttributes Listitem_Attrs where
    fromAttrs as =
	Listitem_Attrs
	  { listitemOverride = possibleA fromAttrToStr "override" as
	  , listitemId = possibleA fromAttrToStr "id" as
	  , listitemLang = possibleA fromAttrToStr "lang" as
	  , listitemRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , listitemRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "override" (listitemOverride v)
	, maybeToAttr toAttrFrStr "id" (listitemId v)
	, maybeToAttr toAttrFrStr "lang" (listitemLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (listitemRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (listitemRole v)
	]
instance XmlAttrType Listitem_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Listitem_Revisionflag_Changed
	    translate "added" = Just Listitem_Revisionflag_Added
	    translate "deleted" = Just Listitem_Revisionflag_Deleted
	    translate "off" = Just Listitem_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Listitem_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Listitem_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Listitem_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Listitem_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Variablelist where
    fromElem (CElem (Elem "variablelist" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Variablelist (fromAttrs as) a b c), rest))
	      (definite fromElem "varlistentry+" "variablelist" cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Variablelist as a b c) =
	[CElem (Elem "variablelist" (toAttrs as) (maybe [] toElem a
						  ++ concatMap toElem b ++
						  toElem c))]
instance XmlAttributes Variablelist_Attrs where
    fromAttrs as =
	Variablelist_Attrs
	  { variablelistTermlength = possibleA fromAttrToStr "termlength" as
	  , variablelistId = possibleA fromAttrToStr "id" as
	  , variablelistLang = possibleA fromAttrToStr "lang" as
	  , variablelistRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , variablelistRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "termlength" (variablelistTermlength v)
	, maybeToAttr toAttrFrStr "id" (variablelistId v)
	, maybeToAttr toAttrFrStr "lang" (variablelistLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (variablelistRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (variablelistRole v)
	]
instance XmlAttrType Variablelist_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Variablelist_Revisionflag_Changed
	    translate "added" = Just Variablelist_Revisionflag_Added
	    translate "deleted" = Just Variablelist_Revisionflag_Deleted
	    translate "off" = Just Variablelist_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Variablelist_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Variablelist_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Variablelist_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Variablelist_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Varlistentry where
    fromElem (CElem (Elem "varlistentry" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Varlistentry (fromAttrs as) a b), rest))
	   (definite fromElem "<listitem>" "varlistentry" ca))
	(definite fromElem "term+" "varlistentry" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Varlistentry as a b) =
	[CElem (Elem "varlistentry" (toAttrs as) (toElem a ++
						  toElem b))]
instance XmlAttributes Varlistentry_Attrs where
    fromAttrs as =
	Varlistentry_Attrs
	  { varlistentryId = possibleA fromAttrToStr "id" as
	  , varlistentryLang = possibleA fromAttrToStr "lang" as
	  , varlistentryRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , varlistentryRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (varlistentryId v)
	, maybeToAttr toAttrFrStr "lang" (varlistentryLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (varlistentryRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (varlistentryRole v)
	]
instance XmlAttrType Varlistentry_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Varlistentry_Revisionflag_Changed
	    translate "added" = Just Varlistentry_Revisionflag_Added
	    translate "deleted" = Just Varlistentry_Revisionflag_Deleted
	    translate "off" = Just Varlistentry_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Varlistentry_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Varlistentry_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Varlistentry_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Varlistentry_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Term where
    fromElem (CElem (Elem "term" as c0):rest) =
	(\(a,ca)->
	   (Just (Term (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Term as a) =
	[CElem (Elem "term" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Term_Attrs where
    fromAttrs as =
	Term_Attrs
	  { termId = possibleA fromAttrToStr "id" as
	  , termLang = possibleA fromAttrToStr "lang" as
	  , termRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , termRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (termId v)
	, maybeToAttr toAttrFrStr "lang" (termLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (termRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (termRole v)
	]
instance XmlContent Term_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Term_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Term_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Term_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Term_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Term_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Term_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Term_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Term_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Term_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Term_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Term_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Term_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Term_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Term_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Term_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Term_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Term_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Term_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Term_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Term_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Term_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Term_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Term_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Term_Str a) = toText a
    toElem (Term_Footnoteref a) = toElem a
    toElem (Term_Xref a) = toElem a
    toElem (Term_Abbrev a) = toElem a
    toElem (Term_Acronym a) = toElem a
    toElem (Term_Citetitle a) = toElem a
    toElem (Term_Emphasis a) = toElem a
    toElem (Term_Footnote a) = toElem a
    toElem (Term_Phrase a) = toElem a
    toElem (Term_Quote a) = toElem a
    toElem (Term_Trademark a) = toElem a
    toElem (Term_Link a) = toElem a
    toElem (Term_Ulink a) = toElem a
    toElem (Term_Command a) = toElem a
    toElem (Term_Computeroutput a) = toElem a
    toElem (Term_Email a) = toElem a
    toElem (Term_Filename a) = toElem a
    toElem (Term_Literal a) = toElem a
    toElem (Term_Option a) = toElem a
    toElem (Term_Replaceable a) = toElem a
    toElem (Term_Systemitem a) = toElem a
    toElem (Term_Userinput a) = toElem a
    toElem (Term_Inlinemediaobject a) = toElem a
instance XmlAttrType Term_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Term_Revisionflag_Changed
	    translate "added" = Just Term_Revisionflag_Added
	    translate "deleted" = Just Term_Revisionflag_Deleted
	    translate "off" = Just Term_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Term_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Term_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Term_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Term_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Example where
    fromElem (CElem (Elem "example" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Example (fromAttrs as) a b c), rest))
	      (definite fromElem "(itemizedlist|orderedlist|variablelist|literallayout|programlisting|para|blockquote|mediaobject|informaltable)+" "example" cb))
	   (fromElem ca))
	(definite fromElem "<title>" "example" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Example as a b c) =
	[CElem (Elem "example" (toAttrs as) (toElem a ++
					     maybe [] toElem b ++ toElem c))]
instance XmlAttributes Example_Attrs where
    fromAttrs as =
	Example_Attrs
	  { exampleLabel = possibleA fromAttrToStr "label" as
	  , exampleWidth = possibleA fromAttrToStr "width" as
	  , exampleId = possibleA fromAttrToStr "id" as
	  , exampleLang = possibleA fromAttrToStr "lang" as
	  , exampleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , exampleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (exampleLabel v)
	, maybeToAttr toAttrFrStr "width" (exampleWidth v)
	, maybeToAttr toAttrFrStr "id" (exampleId v)
	, maybeToAttr toAttrFrStr "lang" (exampleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (exampleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (exampleRole v)
	]
instance XmlAttrType Example_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Example_Revisionflag_Changed
	    translate "added" = Just Example_Revisionflag_Added
	    translate "deleted" = Just Example_Revisionflag_Deleted
	    translate "off" = Just Example_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Example_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Example_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Example_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Example_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Programlisting where
    fromElem (CElem (Elem "programlisting" as c0):rest) =
	(\(a,ca)->
	   (Just (Programlisting (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Programlisting as a) =
	[CElem (Elem "programlisting" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Programlisting_Attrs where
    fromAttrs as =
	Programlisting_Attrs
	  { programlistingWidth = possibleA fromAttrToStr "width" as
	  , programlistingFormat = defaultA fromAttrToTyp Programlisting_Format_Linespecific "format" as
	  , programlistingLinenumbering = possibleA fromAttrToTyp "linenumbering" as
	  , programlistingId = possibleA fromAttrToStr "id" as
	  , programlistingLang = possibleA fromAttrToStr "lang" as
	  , programlistingRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , programlistingRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "width" (programlistingWidth v)
	, defaultToAttr toAttrFrTyp "format" (programlistingFormat v)
	, maybeToAttr toAttrFrTyp "linenumbering" (programlistingLinenumbering v)
	, maybeToAttr toAttrFrStr "id" (programlistingId v)
	, maybeToAttr toAttrFrStr "lang" (programlistingLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (programlistingRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (programlistingRole v)
	]
instance XmlContent Programlisting_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Programlisting_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Programlisting_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Programlisting_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Programlisting_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Programlisting_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Programlisting_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Programlisting_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Programlisting_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Programlisting_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Programlisting_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Programlisting_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Programlisting_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Programlisting_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Programlisting_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Programlisting_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Programlisting_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Programlisting_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Programlisting_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Programlisting_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Programlisting_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Programlisting_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Programlisting_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Programlisting_Inlinemediaobject a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Programlisting_Lineannotation a), rest)
																								(_,_) ->
																								    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Programlisting_Str a) = toText a
    toElem (Programlisting_Footnoteref a) = toElem a
    toElem (Programlisting_Xref a) = toElem a
    toElem (Programlisting_Abbrev a) = toElem a
    toElem (Programlisting_Acronym a) = toElem a
    toElem (Programlisting_Citetitle a) = toElem a
    toElem (Programlisting_Emphasis a) = toElem a
    toElem (Programlisting_Footnote a) = toElem a
    toElem (Programlisting_Phrase a) = toElem a
    toElem (Programlisting_Quote a) = toElem a
    toElem (Programlisting_Trademark a) = toElem a
    toElem (Programlisting_Link a) = toElem a
    toElem (Programlisting_Ulink a) = toElem a
    toElem (Programlisting_Command a) = toElem a
    toElem (Programlisting_Computeroutput a) = toElem a
    toElem (Programlisting_Email a) = toElem a
    toElem (Programlisting_Filename a) = toElem a
    toElem (Programlisting_Literal a) = toElem a
    toElem (Programlisting_Option a) = toElem a
    toElem (Programlisting_Replaceable a) = toElem a
    toElem (Programlisting_Systemitem a) = toElem a
    toElem (Programlisting_Userinput a) = toElem a
    toElem (Programlisting_Inlinemediaobject a) = toElem a
    toElem (Programlisting_Lineannotation a) = toElem a
instance XmlAttrType Programlisting_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "linespecific" = Just Programlisting_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Programlisting_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Programlisting_Linenumbering where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "numbered" = Just Programlisting_Linenumbering_Numbered
	    translate "unnumbered" = Just Programlisting_Linenumbering_Unnumbered
	    translate _ = Nothing
    toAttrFrTyp n Programlisting_Linenumbering_Numbered = Just (n, str2attr "numbered")
    toAttrFrTyp n Programlisting_Linenumbering_Unnumbered = Just (n, str2attr "unnumbered")
instance XmlAttrType Programlisting_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Programlisting_Revisionflag_Changed
	    translate "added" = Just Programlisting_Revisionflag_Added
	    translate "deleted" = Just Programlisting_Revisionflag_Deleted
	    translate "off" = Just Programlisting_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Programlisting_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Programlisting_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Programlisting_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Programlisting_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Literallayout where
    fromElem (CElem (Elem "literallayout" as c0):rest) =
	(\(a,ca)->
	   (Just (Literallayout (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Literallayout as a) =
	[CElem (Elem "literallayout" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Literallayout_Attrs where
    fromAttrs as =
	Literallayout_Attrs
	  { literallayoutWidth = possibleA fromAttrToStr "width" as
	  , literallayoutFormat = defaultA fromAttrToTyp Literallayout_Format_Linespecific "format" as
	  , literallayoutLinenumbering = possibleA fromAttrToTyp "linenumbering" as
	  , literallayoutClass = defaultA fromAttrToTyp Literallayout_Class_Normal "class" as
	  , literallayoutId = possibleA fromAttrToStr "id" as
	  , literallayoutLang = possibleA fromAttrToStr "lang" as
	  , literallayoutRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , literallayoutRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "width" (literallayoutWidth v)
	, defaultToAttr toAttrFrTyp "format" (literallayoutFormat v)
	, maybeToAttr toAttrFrTyp "linenumbering" (literallayoutLinenumbering v)
	, defaultToAttr toAttrFrTyp "class" (literallayoutClass v)
	, maybeToAttr toAttrFrStr "id" (literallayoutId v)
	, maybeToAttr toAttrFrStr "lang" (literallayoutLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (literallayoutRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (literallayoutRole v)
	]
instance XmlContent Literallayout_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Literallayout_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Literallayout_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Literallayout_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Literallayout_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Literallayout_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Literallayout_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Literallayout_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Literallayout_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Literallayout_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Literallayout_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Literallayout_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Literallayout_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Literallayout_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Literallayout_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Literallayout_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Literallayout_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Literallayout_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Literallayout_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Literallayout_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Literallayout_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Literallayout_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Literallayout_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Literallayout_Inlinemediaobject a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Literallayout_Lineannotation a), rest)
																								(_,_) ->
																								    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Literallayout_Str a) = toText a
    toElem (Literallayout_Footnoteref a) = toElem a
    toElem (Literallayout_Xref a) = toElem a
    toElem (Literallayout_Abbrev a) = toElem a
    toElem (Literallayout_Acronym a) = toElem a
    toElem (Literallayout_Citetitle a) = toElem a
    toElem (Literallayout_Emphasis a) = toElem a
    toElem (Literallayout_Footnote a) = toElem a
    toElem (Literallayout_Phrase a) = toElem a
    toElem (Literallayout_Quote a) = toElem a
    toElem (Literallayout_Trademark a) = toElem a
    toElem (Literallayout_Link a) = toElem a
    toElem (Literallayout_Ulink a) = toElem a
    toElem (Literallayout_Command a) = toElem a
    toElem (Literallayout_Computeroutput a) = toElem a
    toElem (Literallayout_Email a) = toElem a
    toElem (Literallayout_Filename a) = toElem a
    toElem (Literallayout_Literal a) = toElem a
    toElem (Literallayout_Option a) = toElem a
    toElem (Literallayout_Replaceable a) = toElem a
    toElem (Literallayout_Systemitem a) = toElem a
    toElem (Literallayout_Userinput a) = toElem a
    toElem (Literallayout_Inlinemediaobject a) = toElem a
    toElem (Literallayout_Lineannotation a) = toElem a
instance XmlAttrType Literallayout_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "linespecific" = Just Literallayout_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Literallayout_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Literallayout_Linenumbering where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "numbered" = Just Literallayout_Linenumbering_Numbered
	    translate "unnumbered" = Just Literallayout_Linenumbering_Unnumbered
	    translate _ = Nothing
    toAttrFrTyp n Literallayout_Linenumbering_Numbered = Just (n, str2attr "numbered")
    toAttrFrTyp n Literallayout_Linenumbering_Unnumbered = Just (n, str2attr "unnumbered")
instance XmlAttrType Literallayout_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "monospaced" = Just Literallayout_Class_Monospaced
	    translate "normal" = Just Literallayout_Class_Normal
	    translate _ = Nothing
    toAttrFrTyp n Literallayout_Class_Monospaced = Just (n, str2attr "monospaced")
    toAttrFrTyp n Literallayout_Class_Normal = Just (n, str2attr "normal")
instance XmlAttrType Literallayout_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Literallayout_Revisionflag_Changed
	    translate "added" = Just Literallayout_Revisionflag_Added
	    translate "deleted" = Just Literallayout_Revisionflag_Deleted
	    translate "off" = Just Literallayout_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Literallayout_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Literallayout_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Literallayout_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Literallayout_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Figure where
    fromElem (CElem (Elem "figure" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Figure (fromAttrs as) a b c), rest))
	      (definite fromElem "(literallayout|programlisting|blockquote|mediaobject|informaltable|link|ulink)+" "figure" cb))
	   (fromElem ca))
	(definite fromElem "<title>" "figure" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Figure as a b c) =
	[CElem (Elem "figure" (toAttrs as) (toElem a ++
					    maybe [] toElem b ++ toElem c))]
instance XmlAttributes Figure_Attrs where
    fromAttrs as =
	Figure_Attrs
	  { figureFloat = defaultA fromAttrToStr "0" "float" as
	  , figurePgwide = possibleA fromAttrToStr "pgwide" as
	  , figureLabel = possibleA fromAttrToStr "label" as
	  , figureId = possibleA fromAttrToStr "id" as
	  , figureLang = possibleA fromAttrToStr "lang" as
	  , figureRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , figureRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrStr "float" (figureFloat v)
	, maybeToAttr toAttrFrStr "pgwide" (figurePgwide v)
	, maybeToAttr toAttrFrStr "label" (figureLabel v)
	, maybeToAttr toAttrFrStr "id" (figureId v)
	, maybeToAttr toAttrFrStr "lang" (figureLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (figureRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (figureRole v)
	]
instance XmlAttrType Figure_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Figure_Revisionflag_Changed
	    translate "added" = Just Figure_Revisionflag_Added
	    translate "deleted" = Just Figure_Revisionflag_Deleted
	    translate "off" = Just Figure_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Figure_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Figure_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Figure_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Figure_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Mediaobject where
    fromElem (CElem (Elem "mediaobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Mediaobject (fromAttrs as) a b c), rest))
	      (fromElem cb))
	   (definite fromElem "(videoobject|audioobject|imageobject|textobject)+" "mediaobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Mediaobject as a b c) =
	[CElem (Elem "mediaobject" (toAttrs as) (maybe [] toElem a
						 ++ toElem b ++
						 maybe [] toElem c))]
instance XmlAttributes Mediaobject_Attrs where
    fromAttrs as =
	Mediaobject_Attrs
	  { mediaobjectId = possibleA fromAttrToStr "id" as
	  , mediaobjectLang = possibleA fromAttrToStr "lang" as
	  , mediaobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , mediaobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (mediaobjectId v)
	, maybeToAttr toAttrFrStr "lang" (mediaobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (mediaobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (mediaobjectRole v)
	]
instance XmlAttrType Mediaobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Mediaobject_Revisionflag_Changed
	    translate "added" = Just Mediaobject_Revisionflag_Added
	    translate "deleted" = Just Mediaobject_Revisionflag_Deleted
	    translate "off" = Just Mediaobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Mediaobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Mediaobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Mediaobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Mediaobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Inlinemediaobject where
    fromElem (CElem (Elem "inlinemediaobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Inlinemediaobject (fromAttrs as) a b), rest))
	   (definite fromElem "(videoobject|audioobject|imageobject|textobject)+" "inlinemediaobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Inlinemediaobject as a b) =
	[CElem (Elem "inlinemediaobject" (toAttrs as) (maybe [] toElem a
						       ++ toElem b))]
instance XmlAttributes Inlinemediaobject_Attrs where
    fromAttrs as =
	Inlinemediaobject_Attrs
	  { inlinemediaobjectId = possibleA fromAttrToStr "id" as
	  , inlinemediaobjectLang = possibleA fromAttrToStr "lang" as
	  , inlinemediaobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , inlinemediaobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (inlinemediaobjectId v)
	, maybeToAttr toAttrFrStr "lang" (inlinemediaobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (inlinemediaobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (inlinemediaobjectRole v)
	]
instance XmlAttrType Inlinemediaobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Inlinemediaobject_Revisionflag_Changed
	    translate "added" = Just Inlinemediaobject_Revisionflag_Added
	    translate "deleted" = Just Inlinemediaobject_Revisionflag_Deleted
	    translate "off" = Just Inlinemediaobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Inlinemediaobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Inlinemediaobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Inlinemediaobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Inlinemediaobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Videoobject where
    fromElem (CElem (Elem "videoobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Videoobject (fromAttrs as) a b), rest))
	   (definite fromElem "<videodata>" "videoobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Videoobject as a b) =
	[CElem (Elem "videoobject" (toAttrs as) (maybe [] toElem a
						 ++ toElem b))]
instance XmlAttributes Videoobject_Attrs where
    fromAttrs as =
	Videoobject_Attrs
	  { videoobjectId = possibleA fromAttrToStr "id" as
	  , videoobjectLang = possibleA fromAttrToStr "lang" as
	  , videoobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , videoobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (videoobjectId v)
	, maybeToAttr toAttrFrStr "lang" (videoobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (videoobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (videoobjectRole v)
	]
instance XmlAttrType Videoobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Videoobject_Revisionflag_Changed
	    translate "added" = Just Videoobject_Revisionflag_Added
	    translate "deleted" = Just Videoobject_Revisionflag_Deleted
	    translate "off" = Just Videoobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Videoobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Videoobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Videoobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Videoobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Audioobject where
    fromElem (CElem (Elem "audioobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Audioobject (fromAttrs as) a b), rest))
	   (definite fromElem "<audiodata>" "audioobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Audioobject as a b) =
	[CElem (Elem "audioobject" (toAttrs as) (maybe [] toElem a
						 ++ toElem b))]
instance XmlAttributes Audioobject_Attrs where
    fromAttrs as =
	Audioobject_Attrs
	  { audioobjectId = possibleA fromAttrToStr "id" as
	  , audioobjectLang = possibleA fromAttrToStr "lang" as
	  , audioobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , audioobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (audioobjectId v)
	, maybeToAttr toAttrFrStr "lang" (audioobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (audioobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (audioobjectRole v)
	]
instance XmlAttrType Audioobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Audioobject_Revisionflag_Changed
	    translate "added" = Just Audioobject_Revisionflag_Added
	    translate "deleted" = Just Audioobject_Revisionflag_Deleted
	    translate "off" = Just Audioobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Audioobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Audioobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Audioobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Audioobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Imageobject where
    fromElem (CElem (Elem "imageobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Imageobject (fromAttrs as) a b), rest))
	   (definite fromElem "<imagedata>" "imageobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Imageobject as a b) =
	[CElem (Elem "imageobject" (toAttrs as) (maybe [] toElem a
						 ++ toElem b))]
instance XmlAttributes Imageobject_Attrs where
    fromAttrs as =
	Imageobject_Attrs
	  { imageobjectId = possibleA fromAttrToStr "id" as
	  , imageobjectLang = possibleA fromAttrToStr "lang" as
	  , imageobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , imageobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (imageobjectId v)
	, maybeToAttr toAttrFrStr "lang" (imageobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (imageobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (imageobjectRole v)
	]
instance XmlAttrType Imageobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Imageobject_Revisionflag_Changed
	    translate "added" = Just Imageobject_Revisionflag_Added
	    translate "deleted" = Just Imageobject_Revisionflag_Deleted
	    translate "off" = Just Imageobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Imageobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Imageobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Imageobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Imageobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Textobject where
    fromElem (CElem (Elem "textobject" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Textobject (fromAttrs as) a b), rest))
	   (definite fromElem "OneOf" "textobject" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Textobject as a b) =
	[CElem (Elem "textobject" (toAttrs as) (maybe [] toElem a
						++ toElem b))]
instance XmlAttributes Textobject_Attrs where
    fromAttrs as =
	Textobject_Attrs
	  { textobjectId = possibleA fromAttrToStr "id" as
	  , textobjectLang = possibleA fromAttrToStr "lang" as
	  , textobjectRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , textobjectRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (textobjectId v)
	, maybeToAttr toAttrFrStr "lang" (textobjectLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (textobjectRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (textobjectRole v)
	]
instance XmlAttrType Textobject_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Textobject_Revisionflag_Changed
	    translate "added" = Just Textobject_Revisionflag_Added
	    translate "deleted" = Just Textobject_Revisionflag_Deleted
	    translate "off" = Just Textobject_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Textobject_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Textobject_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Textobject_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Textobject_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Objectinfo where
    fromElem (CElem (Elem "objectinfo" as c0):rest) =
	(\(a,ca)->
	   (Just (Objectinfo (fromAttrs as) a), rest))
	(definite fromElem "(mediaobject|legalnotice|keywordset|subjectset|abbrev|abstract|author|authorgroup|bibliomisc|copyright|corpauthor|date|edition|editor|issuenum|othercredit|pubdate|publishername|releaseinfo|revhistory|subtitle|title|titleabbrev|volumenum|citetitle|honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "objectinfo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Objectinfo as a) =
	[CElem (Elem "objectinfo" (toAttrs as) (toElem a))]
instance XmlAttributes Objectinfo_Attrs where
    fromAttrs as =
	Objectinfo_Attrs
	  { objectinfoId = possibleA fromAttrToStr "id" as
	  , objectinfoLang = possibleA fromAttrToStr "lang" as
	  , objectinfoRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , objectinfoRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (objectinfoId v)
	, maybeToAttr toAttrFrStr "lang" (objectinfoLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (objectinfoRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (objectinfoRole v)
	]
instance XmlAttrType Objectinfo_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Objectinfo_Revisionflag_Changed
	    translate "added" = Just Objectinfo_Revisionflag_Added
	    translate "deleted" = Just Objectinfo_Revisionflag_Deleted
	    translate "off" = Just Objectinfo_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Objectinfo_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Objectinfo_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Objectinfo_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Objectinfo_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Videodata where
    fromElem (CElem (Elem "videodata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "videodata" (toAttrs as) [])]
instance XmlAttributes Videodata where
    fromAttrs as =
	Videodata
	  { videodataWidth = possibleA fromAttrToStr "width" as
	  , videodataContentwidth = possibleA fromAttrToStr "contentwidth" as
	  , videodataDepth = possibleA fromAttrToStr "depth" as
	  , videodataContentdepth = possibleA fromAttrToStr "contentdepth" as
	  , videodataAlign = possibleA fromAttrToTyp "align" as
	  , videodataValign = possibleA fromAttrToTyp "valign" as
	  , videodataScale = possibleA fromAttrToStr "scale" as
	  , videodataScalefit = possibleA fromAttrToStr "scalefit" as
	  , videodataEntityref = possibleA fromAttrToStr "entityref" as
	  , videodataFileref = possibleA fromAttrToStr "fileref" as
	  , videodataFormat = possibleA fromAttrToTyp "format" as
	  , videodataSrccredit = possibleA fromAttrToStr "srccredit" as
	  , videodataId = possibleA fromAttrToStr "id" as
	  , videodataLang = possibleA fromAttrToStr "lang" as
	  , videodataRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , videodataRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "width" (videodataWidth v)
	, maybeToAttr toAttrFrStr "contentwidth" (videodataContentwidth v)
	, maybeToAttr toAttrFrStr "depth" (videodataDepth v)
	, maybeToAttr toAttrFrStr "contentdepth" (videodataContentdepth v)
	, maybeToAttr toAttrFrTyp "align" (videodataAlign v)
	, maybeToAttr toAttrFrTyp "valign" (videodataValign v)
	, maybeToAttr toAttrFrStr "scale" (videodataScale v)
	, maybeToAttr toAttrFrStr "scalefit" (videodataScalefit v)
	, maybeToAttr toAttrFrStr "entityref" (videodataEntityref v)
	, maybeToAttr toAttrFrStr "fileref" (videodataFileref v)
	, maybeToAttr toAttrFrTyp "format" (videodataFormat v)
	, maybeToAttr toAttrFrStr "srccredit" (videodataSrccredit v)
	, maybeToAttr toAttrFrStr "id" (videodataId v)
	, maybeToAttr toAttrFrStr "lang" (videodataLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (videodataRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (videodataRole v)
	]
instance XmlAttrType Videodata_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Videodata_Align_Left
	    translate "right" = Just Videodata_Align_Right
	    translate "center" = Just Videodata_Align_Center
	    translate _ = Nothing
    toAttrFrTyp n Videodata_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Videodata_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Videodata_Align_Center = Just (n, str2attr "center")
instance XmlAttrType Videodata_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Videodata_Valign_Top
	    translate "middle" = Just Videodata_Valign_Middle
	    translate "bottom" = Just Videodata_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Videodata_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Videodata_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Videodata_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Videodata_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "BMP" = Just Videodata_Format_BMP
	    translate "CGM-CHAR" = Just Videodata_Format_CGM_CHAR
	    translate "CGM-BINARY" = Just Videodata_Format_CGM_BINARY
	    translate "CGM-CLEAR" = Just Videodata_Format_CGM_CLEAR
	    translate "DITROFF" = Just Videodata_Format_DITROFF
	    translate "DVI" = Just Videodata_Format_DVI
	    translate "EPS" = Just Videodata_Format_EPS
	    translate "EQN" = Just Videodata_Format_EQN
	    translate "FAX" = Just Videodata_Format_FAX
	    translate "GIF" = Just Videodata_Format_GIF
	    translate "GIF87a" = Just Videodata_Format_GIF87a
	    translate "GIF89a" = Just Videodata_Format_GIF89a
	    translate "JPG" = Just Videodata_Format_JPG
	    translate "JPEG" = Just Videodata_Format_JPEG
	    translate "IGES" = Just Videodata_Format_IGES
	    translate "PCX" = Just Videodata_Format_PCX
	    translate "PIC" = Just Videodata_Format_PIC
	    translate "PNG" = Just Videodata_Format_PNG
	    translate "PS" = Just Videodata_Format_PS
	    translate "SGML" = Just Videodata_Format_SGML
	    translate "TBL" = Just Videodata_Format_TBL
	    translate "TEX" = Just Videodata_Format_TEX
	    translate "TIFF" = Just Videodata_Format_TIFF
	    translate "WMF" = Just Videodata_Format_WMF
	    translate "WPG" = Just Videodata_Format_WPG
	    translate "SVG" = Just Videodata_Format_SVG
	    translate "linespecific" = Just Videodata_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Videodata_Format_BMP = Just (n, str2attr "BMP")
    toAttrFrTyp n Videodata_Format_CGM_CHAR = Just (n, str2attr "CGM-CHAR")
    toAttrFrTyp n Videodata_Format_CGM_BINARY = Just (n, str2attr "CGM-BINARY")
    toAttrFrTyp n Videodata_Format_CGM_CLEAR = Just (n, str2attr "CGM-CLEAR")
    toAttrFrTyp n Videodata_Format_DITROFF = Just (n, str2attr "DITROFF")
    toAttrFrTyp n Videodata_Format_DVI = Just (n, str2attr "DVI")
    toAttrFrTyp n Videodata_Format_EPS = Just (n, str2attr "EPS")
    toAttrFrTyp n Videodata_Format_EQN = Just (n, str2attr "EQN")
    toAttrFrTyp n Videodata_Format_FAX = Just (n, str2attr "FAX")
    toAttrFrTyp n Videodata_Format_GIF = Just (n, str2attr "GIF")
    toAttrFrTyp n Videodata_Format_GIF87a = Just (n, str2attr "GIF87a")
    toAttrFrTyp n Videodata_Format_GIF89a = Just (n, str2attr "GIF89a")
    toAttrFrTyp n Videodata_Format_JPG = Just (n, str2attr "JPG")
    toAttrFrTyp n Videodata_Format_JPEG = Just (n, str2attr "JPEG")
    toAttrFrTyp n Videodata_Format_IGES = Just (n, str2attr "IGES")
    toAttrFrTyp n Videodata_Format_PCX = Just (n, str2attr "PCX")
    toAttrFrTyp n Videodata_Format_PIC = Just (n, str2attr "PIC")
    toAttrFrTyp n Videodata_Format_PNG = Just (n, str2attr "PNG")
    toAttrFrTyp n Videodata_Format_PS = Just (n, str2attr "PS")
    toAttrFrTyp n Videodata_Format_SGML = Just (n, str2attr "SGML")
    toAttrFrTyp n Videodata_Format_TBL = Just (n, str2attr "TBL")
    toAttrFrTyp n Videodata_Format_TEX = Just (n, str2attr "TEX")
    toAttrFrTyp n Videodata_Format_TIFF = Just (n, str2attr "TIFF")
    toAttrFrTyp n Videodata_Format_WMF = Just (n, str2attr "WMF")
    toAttrFrTyp n Videodata_Format_WPG = Just (n, str2attr "WPG")
    toAttrFrTyp n Videodata_Format_SVG = Just (n, str2attr "SVG")
    toAttrFrTyp n Videodata_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Videodata_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Videodata_Revisionflag_Changed
	    translate "added" = Just Videodata_Revisionflag_Added
	    translate "deleted" = Just Videodata_Revisionflag_Deleted
	    translate "off" = Just Videodata_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Videodata_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Videodata_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Videodata_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Videodata_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Audiodata where
    fromElem (CElem (Elem "audiodata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "audiodata" (toAttrs as) [])]
instance XmlAttributes Audiodata where
    fromAttrs as =
	Audiodata
	  { audiodataEntityref = possibleA fromAttrToStr "entityref" as
	  , audiodataFileref = possibleA fromAttrToStr "fileref" as
	  , audiodataFormat = possibleA fromAttrToTyp "format" as
	  , audiodataSrccredit = possibleA fromAttrToStr "srccredit" as
	  , audiodataId = possibleA fromAttrToStr "id" as
	  , audiodataLang = possibleA fromAttrToStr "lang" as
	  , audiodataRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , audiodataRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "entityref" (audiodataEntityref v)
	, maybeToAttr toAttrFrStr "fileref" (audiodataFileref v)
	, maybeToAttr toAttrFrTyp "format" (audiodataFormat v)
	, maybeToAttr toAttrFrStr "srccredit" (audiodataSrccredit v)
	, maybeToAttr toAttrFrStr "id" (audiodataId v)
	, maybeToAttr toAttrFrStr "lang" (audiodataLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (audiodataRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (audiodataRole v)
	]
instance XmlAttrType Audiodata_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "BMP" = Just Audiodata_Format_BMP
	    translate "CGM-CHAR" = Just Audiodata_Format_CGM_CHAR
	    translate "CGM-BINARY" = Just Audiodata_Format_CGM_BINARY
	    translate "CGM-CLEAR" = Just Audiodata_Format_CGM_CLEAR
	    translate "DITROFF" = Just Audiodata_Format_DITROFF
	    translate "DVI" = Just Audiodata_Format_DVI
	    translate "EPS" = Just Audiodata_Format_EPS
	    translate "EQN" = Just Audiodata_Format_EQN
	    translate "FAX" = Just Audiodata_Format_FAX
	    translate "GIF" = Just Audiodata_Format_GIF
	    translate "GIF87a" = Just Audiodata_Format_GIF87a
	    translate "GIF89a" = Just Audiodata_Format_GIF89a
	    translate "JPG" = Just Audiodata_Format_JPG
	    translate "JPEG" = Just Audiodata_Format_JPEG
	    translate "IGES" = Just Audiodata_Format_IGES
	    translate "PCX" = Just Audiodata_Format_PCX
	    translate "PIC" = Just Audiodata_Format_PIC
	    translate "PNG" = Just Audiodata_Format_PNG
	    translate "PS" = Just Audiodata_Format_PS
	    translate "SGML" = Just Audiodata_Format_SGML
	    translate "TBL" = Just Audiodata_Format_TBL
	    translate "TEX" = Just Audiodata_Format_TEX
	    translate "TIFF" = Just Audiodata_Format_TIFF
	    translate "WMF" = Just Audiodata_Format_WMF
	    translate "WPG" = Just Audiodata_Format_WPG
	    translate "SVG" = Just Audiodata_Format_SVG
	    translate "linespecific" = Just Audiodata_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Audiodata_Format_BMP = Just (n, str2attr "BMP")
    toAttrFrTyp n Audiodata_Format_CGM_CHAR = Just (n, str2attr "CGM-CHAR")
    toAttrFrTyp n Audiodata_Format_CGM_BINARY = Just (n, str2attr "CGM-BINARY")
    toAttrFrTyp n Audiodata_Format_CGM_CLEAR = Just (n, str2attr "CGM-CLEAR")
    toAttrFrTyp n Audiodata_Format_DITROFF = Just (n, str2attr "DITROFF")
    toAttrFrTyp n Audiodata_Format_DVI = Just (n, str2attr "DVI")
    toAttrFrTyp n Audiodata_Format_EPS = Just (n, str2attr "EPS")
    toAttrFrTyp n Audiodata_Format_EQN = Just (n, str2attr "EQN")
    toAttrFrTyp n Audiodata_Format_FAX = Just (n, str2attr "FAX")
    toAttrFrTyp n Audiodata_Format_GIF = Just (n, str2attr "GIF")
    toAttrFrTyp n Audiodata_Format_GIF87a = Just (n, str2attr "GIF87a")
    toAttrFrTyp n Audiodata_Format_GIF89a = Just (n, str2attr "GIF89a")
    toAttrFrTyp n Audiodata_Format_JPG = Just (n, str2attr "JPG")
    toAttrFrTyp n Audiodata_Format_JPEG = Just (n, str2attr "JPEG")
    toAttrFrTyp n Audiodata_Format_IGES = Just (n, str2attr "IGES")
    toAttrFrTyp n Audiodata_Format_PCX = Just (n, str2attr "PCX")
    toAttrFrTyp n Audiodata_Format_PIC = Just (n, str2attr "PIC")
    toAttrFrTyp n Audiodata_Format_PNG = Just (n, str2attr "PNG")
    toAttrFrTyp n Audiodata_Format_PS = Just (n, str2attr "PS")
    toAttrFrTyp n Audiodata_Format_SGML = Just (n, str2attr "SGML")
    toAttrFrTyp n Audiodata_Format_TBL = Just (n, str2attr "TBL")
    toAttrFrTyp n Audiodata_Format_TEX = Just (n, str2attr "TEX")
    toAttrFrTyp n Audiodata_Format_TIFF = Just (n, str2attr "TIFF")
    toAttrFrTyp n Audiodata_Format_WMF = Just (n, str2attr "WMF")
    toAttrFrTyp n Audiodata_Format_WPG = Just (n, str2attr "WPG")
    toAttrFrTyp n Audiodata_Format_SVG = Just (n, str2attr "SVG")
    toAttrFrTyp n Audiodata_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Audiodata_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Audiodata_Revisionflag_Changed
	    translate "added" = Just Audiodata_Revisionflag_Added
	    translate "deleted" = Just Audiodata_Revisionflag_Deleted
	    translate "off" = Just Audiodata_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Audiodata_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Audiodata_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Audiodata_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Audiodata_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Imagedata where
    fromElem (CElem (Elem "imagedata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "imagedata" (toAttrs as) [])]
instance XmlAttributes Imagedata where
    fromAttrs as =
	Imagedata
	  { imagedataWidth = possibleA fromAttrToStr "width" as
	  , imagedataContentwidth = possibleA fromAttrToStr "contentwidth" as
	  , imagedataDepth = possibleA fromAttrToStr "depth" as
	  , imagedataContentdepth = possibleA fromAttrToStr "contentdepth" as
	  , imagedataAlign = possibleA fromAttrToTyp "align" as
	  , imagedataValign = possibleA fromAttrToTyp "valign" as
	  , imagedataScale = possibleA fromAttrToStr "scale" as
	  , imagedataScalefit = possibleA fromAttrToStr "scalefit" as
	  , imagedataEntityref = possibleA fromAttrToStr "entityref" as
	  , imagedataFileref = possibleA fromAttrToStr "fileref" as
	  , imagedataFormat = possibleA fromAttrToTyp "format" as
	  , imagedataSrccredit = possibleA fromAttrToStr "srccredit" as
	  , imagedataId = possibleA fromAttrToStr "id" as
	  , imagedataLang = possibleA fromAttrToStr "lang" as
	  , imagedataRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , imagedataRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "width" (imagedataWidth v)
	, maybeToAttr toAttrFrStr "contentwidth" (imagedataContentwidth v)
	, maybeToAttr toAttrFrStr "depth" (imagedataDepth v)
	, maybeToAttr toAttrFrStr "contentdepth" (imagedataContentdepth v)
	, maybeToAttr toAttrFrTyp "align" (imagedataAlign v)
	, maybeToAttr toAttrFrTyp "valign" (imagedataValign v)
	, maybeToAttr toAttrFrStr "scale" (imagedataScale v)
	, maybeToAttr toAttrFrStr "scalefit" (imagedataScalefit v)
	, maybeToAttr toAttrFrStr "entityref" (imagedataEntityref v)
	, maybeToAttr toAttrFrStr "fileref" (imagedataFileref v)
	, maybeToAttr toAttrFrTyp "format" (imagedataFormat v)
	, maybeToAttr toAttrFrStr "srccredit" (imagedataSrccredit v)
	, maybeToAttr toAttrFrStr "id" (imagedataId v)
	, maybeToAttr toAttrFrStr "lang" (imagedataLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (imagedataRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (imagedataRole v)
	]
instance XmlAttrType Imagedata_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Imagedata_Align_Left
	    translate "right" = Just Imagedata_Align_Right
	    translate "center" = Just Imagedata_Align_Center
	    translate _ = Nothing
    toAttrFrTyp n Imagedata_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Imagedata_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Imagedata_Align_Center = Just (n, str2attr "center")
instance XmlAttrType Imagedata_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Imagedata_Valign_Top
	    translate "middle" = Just Imagedata_Valign_Middle
	    translate "bottom" = Just Imagedata_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Imagedata_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Imagedata_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Imagedata_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Imagedata_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "BMP" = Just Imagedata_Format_BMP
	    translate "CGM-CHAR" = Just Imagedata_Format_CGM_CHAR
	    translate "CGM-BINARY" = Just Imagedata_Format_CGM_BINARY
	    translate "CGM-CLEAR" = Just Imagedata_Format_CGM_CLEAR
	    translate "DITROFF" = Just Imagedata_Format_DITROFF
	    translate "DVI" = Just Imagedata_Format_DVI
	    translate "EPS" = Just Imagedata_Format_EPS
	    translate "EQN" = Just Imagedata_Format_EQN
	    translate "FAX" = Just Imagedata_Format_FAX
	    translate "GIF" = Just Imagedata_Format_GIF
	    translate "GIF87a" = Just Imagedata_Format_GIF87a
	    translate "GIF89a" = Just Imagedata_Format_GIF89a
	    translate "JPG" = Just Imagedata_Format_JPG
	    translate "JPEG" = Just Imagedata_Format_JPEG
	    translate "IGES" = Just Imagedata_Format_IGES
	    translate "PCX" = Just Imagedata_Format_PCX
	    translate "PIC" = Just Imagedata_Format_PIC
	    translate "PNG" = Just Imagedata_Format_PNG
	    translate "PS" = Just Imagedata_Format_PS
	    translate "SGML" = Just Imagedata_Format_SGML
	    translate "TBL" = Just Imagedata_Format_TBL
	    translate "TEX" = Just Imagedata_Format_TEX
	    translate "TIFF" = Just Imagedata_Format_TIFF
	    translate "WMF" = Just Imagedata_Format_WMF
	    translate "WPG" = Just Imagedata_Format_WPG
	    translate "SVG" = Just Imagedata_Format_SVG
	    translate "linespecific" = Just Imagedata_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Imagedata_Format_BMP = Just (n, str2attr "BMP")
    toAttrFrTyp n Imagedata_Format_CGM_CHAR = Just (n, str2attr "CGM-CHAR")
    toAttrFrTyp n Imagedata_Format_CGM_BINARY = Just (n, str2attr "CGM-BINARY")
    toAttrFrTyp n Imagedata_Format_CGM_CLEAR = Just (n, str2attr "CGM-CLEAR")
    toAttrFrTyp n Imagedata_Format_DITROFF = Just (n, str2attr "DITROFF")
    toAttrFrTyp n Imagedata_Format_DVI = Just (n, str2attr "DVI")
    toAttrFrTyp n Imagedata_Format_EPS = Just (n, str2attr "EPS")
    toAttrFrTyp n Imagedata_Format_EQN = Just (n, str2attr "EQN")
    toAttrFrTyp n Imagedata_Format_FAX = Just (n, str2attr "FAX")
    toAttrFrTyp n Imagedata_Format_GIF = Just (n, str2attr "GIF")
    toAttrFrTyp n Imagedata_Format_GIF87a = Just (n, str2attr "GIF87a")
    toAttrFrTyp n Imagedata_Format_GIF89a = Just (n, str2attr "GIF89a")
    toAttrFrTyp n Imagedata_Format_JPG = Just (n, str2attr "JPG")
    toAttrFrTyp n Imagedata_Format_JPEG = Just (n, str2attr "JPEG")
    toAttrFrTyp n Imagedata_Format_IGES = Just (n, str2attr "IGES")
    toAttrFrTyp n Imagedata_Format_PCX = Just (n, str2attr "PCX")
    toAttrFrTyp n Imagedata_Format_PIC = Just (n, str2attr "PIC")
    toAttrFrTyp n Imagedata_Format_PNG = Just (n, str2attr "PNG")
    toAttrFrTyp n Imagedata_Format_PS = Just (n, str2attr "PS")
    toAttrFrTyp n Imagedata_Format_SGML = Just (n, str2attr "SGML")
    toAttrFrTyp n Imagedata_Format_TBL = Just (n, str2attr "TBL")
    toAttrFrTyp n Imagedata_Format_TEX = Just (n, str2attr "TEX")
    toAttrFrTyp n Imagedata_Format_TIFF = Just (n, str2attr "TIFF")
    toAttrFrTyp n Imagedata_Format_WMF = Just (n, str2attr "WMF")
    toAttrFrTyp n Imagedata_Format_WPG = Just (n, str2attr "WPG")
    toAttrFrTyp n Imagedata_Format_SVG = Just (n, str2attr "SVG")
    toAttrFrTyp n Imagedata_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Imagedata_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Imagedata_Revisionflag_Changed
	    translate "added" = Just Imagedata_Revisionflag_Added
	    translate "deleted" = Just Imagedata_Revisionflag_Deleted
	    translate "off" = Just Imagedata_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Imagedata_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Imagedata_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Imagedata_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Imagedata_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Textdata where
    fromElem (CElem (Elem "textdata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "textdata" (toAttrs as) [])]
instance XmlAttributes Textdata where
    fromAttrs as =
	Textdata
	  { textdataEncoding = possibleA fromAttrToStr "encoding" as
	  , textdataEntityref = possibleA fromAttrToStr "entityref" as
	  , textdataFileref = possibleA fromAttrToStr "fileref" as
	  , textdataFormat = possibleA fromAttrToTyp "format" as
	  , textdataSrccredit = possibleA fromAttrToStr "srccredit" as
	  , textdataId = possibleA fromAttrToStr "id" as
	  , textdataLang = possibleA fromAttrToStr "lang" as
	  , textdataRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , textdataRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "encoding" (textdataEncoding v)
	, maybeToAttr toAttrFrStr "entityref" (textdataEntityref v)
	, maybeToAttr toAttrFrStr "fileref" (textdataFileref v)
	, maybeToAttr toAttrFrTyp "format" (textdataFormat v)
	, maybeToAttr toAttrFrStr "srccredit" (textdataSrccredit v)
	, maybeToAttr toAttrFrStr "id" (textdataId v)
	, maybeToAttr toAttrFrStr "lang" (textdataLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (textdataRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (textdataRole v)
	]
instance XmlAttrType Textdata_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "BMP" = Just Textdata_Format_BMP
	    translate "CGM-CHAR" = Just Textdata_Format_CGM_CHAR
	    translate "CGM-BINARY" = Just Textdata_Format_CGM_BINARY
	    translate "CGM-CLEAR" = Just Textdata_Format_CGM_CLEAR
	    translate "DITROFF" = Just Textdata_Format_DITROFF
	    translate "DVI" = Just Textdata_Format_DVI
	    translate "EPS" = Just Textdata_Format_EPS
	    translate "EQN" = Just Textdata_Format_EQN
	    translate "FAX" = Just Textdata_Format_FAX
	    translate "GIF" = Just Textdata_Format_GIF
	    translate "GIF87a" = Just Textdata_Format_GIF87a
	    translate "GIF89a" = Just Textdata_Format_GIF89a
	    translate "JPG" = Just Textdata_Format_JPG
	    translate "JPEG" = Just Textdata_Format_JPEG
	    translate "IGES" = Just Textdata_Format_IGES
	    translate "PCX" = Just Textdata_Format_PCX
	    translate "PIC" = Just Textdata_Format_PIC
	    translate "PNG" = Just Textdata_Format_PNG
	    translate "PS" = Just Textdata_Format_PS
	    translate "SGML" = Just Textdata_Format_SGML
	    translate "TBL" = Just Textdata_Format_TBL
	    translate "TEX" = Just Textdata_Format_TEX
	    translate "TIFF" = Just Textdata_Format_TIFF
	    translate "WMF" = Just Textdata_Format_WMF
	    translate "WPG" = Just Textdata_Format_WPG
	    translate "SVG" = Just Textdata_Format_SVG
	    translate "linespecific" = Just Textdata_Format_Linespecific
	    translate _ = Nothing
    toAttrFrTyp n Textdata_Format_BMP = Just (n, str2attr "BMP")
    toAttrFrTyp n Textdata_Format_CGM_CHAR = Just (n, str2attr "CGM-CHAR")
    toAttrFrTyp n Textdata_Format_CGM_BINARY = Just (n, str2attr "CGM-BINARY")
    toAttrFrTyp n Textdata_Format_CGM_CLEAR = Just (n, str2attr "CGM-CLEAR")
    toAttrFrTyp n Textdata_Format_DITROFF = Just (n, str2attr "DITROFF")
    toAttrFrTyp n Textdata_Format_DVI = Just (n, str2attr "DVI")
    toAttrFrTyp n Textdata_Format_EPS = Just (n, str2attr "EPS")
    toAttrFrTyp n Textdata_Format_EQN = Just (n, str2attr "EQN")
    toAttrFrTyp n Textdata_Format_FAX = Just (n, str2attr "FAX")
    toAttrFrTyp n Textdata_Format_GIF = Just (n, str2attr "GIF")
    toAttrFrTyp n Textdata_Format_GIF87a = Just (n, str2attr "GIF87a")
    toAttrFrTyp n Textdata_Format_GIF89a = Just (n, str2attr "GIF89a")
    toAttrFrTyp n Textdata_Format_JPG = Just (n, str2attr "JPG")
    toAttrFrTyp n Textdata_Format_JPEG = Just (n, str2attr "JPEG")
    toAttrFrTyp n Textdata_Format_IGES = Just (n, str2attr "IGES")
    toAttrFrTyp n Textdata_Format_PCX = Just (n, str2attr "PCX")
    toAttrFrTyp n Textdata_Format_PIC = Just (n, str2attr "PIC")
    toAttrFrTyp n Textdata_Format_PNG = Just (n, str2attr "PNG")
    toAttrFrTyp n Textdata_Format_PS = Just (n, str2attr "PS")
    toAttrFrTyp n Textdata_Format_SGML = Just (n, str2attr "SGML")
    toAttrFrTyp n Textdata_Format_TBL = Just (n, str2attr "TBL")
    toAttrFrTyp n Textdata_Format_TEX = Just (n, str2attr "TEX")
    toAttrFrTyp n Textdata_Format_TIFF = Just (n, str2attr "TIFF")
    toAttrFrTyp n Textdata_Format_WMF = Just (n, str2attr "WMF")
    toAttrFrTyp n Textdata_Format_WPG = Just (n, str2attr "WPG")
    toAttrFrTyp n Textdata_Format_SVG = Just (n, str2attr "SVG")
    toAttrFrTyp n Textdata_Format_Linespecific = Just (n, str2attr "linespecific")
instance XmlAttrType Textdata_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Textdata_Revisionflag_Changed
	    translate "added" = Just Textdata_Revisionflag_Added
	    translate "deleted" = Just Textdata_Revisionflag_Deleted
	    translate "off" = Just Textdata_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Textdata_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Textdata_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Textdata_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Textdata_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Caption where
    fromElem (CElem (Elem "caption" as c0):rest) =
	(\(a,ca)->
	   (Just (Caption (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Caption as a) =
	[CElem (Elem "caption" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Caption_Attrs where
    fromAttrs as =
	Caption_Attrs
	  { captionId = possibleA fromAttrToStr "id" as
	  , captionLang = possibleA fromAttrToStr "lang" as
	  , captionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , captionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (captionId v)
	, maybeToAttr toAttrFrStr "lang" (captionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (captionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (captionRole v)
	]
instance XmlContent Caption_ where
    fromElem c0 =
	case (fromElem c0) of
	(Just a,rest) -> (Just (Caption_Itemizedlist a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Caption_Orderedlist a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Caption_Variablelist a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Caption_Note a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Caption_Literallayout a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Caption_Programlisting a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Caption_Para a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Caption_Blockquote a), rest)
								(_,_) ->
								    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Caption_Itemizedlist a) = toElem a
    toElem (Caption_Orderedlist a) = toElem a
    toElem (Caption_Variablelist a) = toElem a
    toElem (Caption_Note a) = toElem a
    toElem (Caption_Literallayout a) = toElem a
    toElem (Caption_Programlisting a) = toElem a
    toElem (Caption_Para a) = toElem a
    toElem (Caption_Blockquote a) = toElem a
instance XmlAttrType Caption_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Caption_Revisionflag_Changed
	    translate "added" = Just Caption_Revisionflag_Added
	    translate "deleted" = Just Caption_Revisionflag_Deleted
	    translate "off" = Just Caption_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Caption_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Caption_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Caption_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Caption_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Table where
    fromElem (CElem (Elem "table" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Table (fromAttrs as) a b), rest))
	   (definite fromElem "OneOf" "table" ca))
	(definite fromElem "<title>" "table" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Table as a b) =
	[CElem (Elem "table" (toAttrs as) (toElem a ++
					   toElem b))]
instance XmlAttributes Table_Attrs where
    fromAttrs as =
	Table_Attrs
	  { tableFrame = possibleA fromAttrToTyp "frame" as
	  , tableColsep = possibleA fromAttrToStr "colsep" as
	  , tableRowsep = possibleA fromAttrToStr "rowsep" as
	  , tableTabstyle = possibleA fromAttrToStr "tabstyle" as
	  , tableTocentry = possibleA fromAttrToStr "tocentry" as
	  , tableShortentry = possibleA fromAttrToStr "shortentry" as
	  , tableOrient = possibleA fromAttrToTyp "orient" as
	  , tablePgwide = possibleA fromAttrToStr "pgwide" as
	  , tableLabel = possibleA fromAttrToStr "label" as
	  , tableId = possibleA fromAttrToStr "id" as
	  , tableLang = possibleA fromAttrToStr "lang" as
	  , tableRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , tableRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "frame" (tableFrame v)
	, maybeToAttr toAttrFrStr "colsep" (tableColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (tableRowsep v)
	, maybeToAttr toAttrFrStr "tabstyle" (tableTabstyle v)
	, maybeToAttr toAttrFrStr "tocentry" (tableTocentry v)
	, maybeToAttr toAttrFrStr "shortentry" (tableShortentry v)
	, maybeToAttr toAttrFrTyp "orient" (tableOrient v)
	, maybeToAttr toAttrFrStr "pgwide" (tablePgwide v)
	, maybeToAttr toAttrFrStr "label" (tableLabel v)
	, maybeToAttr toAttrFrStr "id" (tableId v)
	, maybeToAttr toAttrFrStr "lang" (tableLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (tableRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (tableRole v)
	]
instance XmlAttrType Table_Frame where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Table_Frame_Top
	    translate "bottom" = Just Table_Frame_Bottom
	    translate "topbot" = Just Table_Frame_Topbot
	    translate "all" = Just Table_Frame_All
	    translate "sides" = Just Table_Frame_Sides
	    translate "none" = Just Table_Frame_None
	    translate _ = Nothing
    toAttrFrTyp n Table_Frame_Top = Just (n, str2attr "top")
    toAttrFrTyp n Table_Frame_Bottom = Just (n, str2attr "bottom")
    toAttrFrTyp n Table_Frame_Topbot = Just (n, str2attr "topbot")
    toAttrFrTyp n Table_Frame_All = Just (n, str2attr "all")
    toAttrFrTyp n Table_Frame_Sides = Just (n, str2attr "sides")
    toAttrFrTyp n Table_Frame_None = Just (n, str2attr "none")
instance XmlAttrType Table_Orient where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "port" = Just Table_Orient_Port
	    translate "land" = Just Table_Orient_Land
	    translate _ = Nothing
    toAttrFrTyp n Table_Orient_Port = Just (n, str2attr "port")
    toAttrFrTyp n Table_Orient_Land = Just (n, str2attr "land")
instance XmlAttrType Table_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Table_Revisionflag_Changed
	    translate "added" = Just Table_Revisionflag_Added
	    translate "deleted" = Just Table_Revisionflag_Deleted
	    translate "off" = Just Table_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Table_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Table_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Table_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Table_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Tgroup where
    fromElem (CElem (Elem "tgroup" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (Just (Tgroup (fromAttrs as) a b c d e), rest))
		    (definite fromElem "<tbody>" "tgroup" cd))
		 (fromElem cc))
	      (fromElem cb))
	   (many fromElem ca))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Tgroup as a b c d e) =
	[CElem (Elem "tgroup" (toAttrs as) (concatMap toElem a
					    ++ concatMap toElem b ++
					    maybe [] toElem c ++
					    maybe [] toElem d ++ toElem e))]
instance XmlAttributes Tgroup_Attrs where
    fromAttrs as =
	Tgroup_Attrs
	  { tgroupCols = definiteA fromAttrToStr "tgroup" "cols" as
	  , tgroupTgroupstyle = possibleA fromAttrToStr "tgroupstyle" as
	  , tgroupColsep = possibleA fromAttrToStr "colsep" as
	  , tgroupRowsep = possibleA fromAttrToStr "rowsep" as
	  , tgroupAlign = possibleA fromAttrToTyp "align" as
	  , tgroupChar = possibleA fromAttrToStr "char" as
	  , tgroupCharoff = possibleA fromAttrToStr "charoff" as
	  , tgroupId = possibleA fromAttrToStr "id" as
	  , tgroupLang = possibleA fromAttrToStr "lang" as
	  , tgroupRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , tgroupRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "cols" (tgroupCols v)
	, maybeToAttr toAttrFrStr "tgroupstyle" (tgroupTgroupstyle v)
	, maybeToAttr toAttrFrStr "colsep" (tgroupColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (tgroupRowsep v)
	, maybeToAttr toAttrFrTyp "align" (tgroupAlign v)
	, maybeToAttr toAttrFrStr "char" (tgroupChar v)
	, maybeToAttr toAttrFrStr "charoff" (tgroupCharoff v)
	, maybeToAttr toAttrFrStr "id" (tgroupId v)
	, maybeToAttr toAttrFrStr "lang" (tgroupLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (tgroupRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (tgroupRole v)
	]
instance XmlAttrType Tgroup_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Tgroup_Align_Left
	    translate "right" = Just Tgroup_Align_Right
	    translate "center" = Just Tgroup_Align_Center
	    translate "justify" = Just Tgroup_Align_Justify
	    translate "char" = Just Tgroup_Align_Char
	    translate _ = Nothing
    toAttrFrTyp n Tgroup_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Tgroup_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Tgroup_Align_Center = Just (n, str2attr "center")
    toAttrFrTyp n Tgroup_Align_Justify = Just (n, str2attr "justify")
    toAttrFrTyp n Tgroup_Align_Char = Just (n, str2attr "char")
instance XmlAttrType Tgroup_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Tgroup_Revisionflag_Changed
	    translate "added" = Just Tgroup_Revisionflag_Added
	    translate "deleted" = Just Tgroup_Revisionflag_Deleted
	    translate "off" = Just Tgroup_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Tgroup_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Tgroup_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Tgroup_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Tgroup_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Colspec where
    fromElem (CElem (Elem "colspec" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "colspec" (toAttrs as) [])]
instance XmlAttributes Colspec where
    fromAttrs as =
	Colspec
	  { colspecColnum = possibleA fromAttrToStr "colnum" as
	  , colspecColname = possibleA fromAttrToStr "colname" as
	  , colspecColwidth = possibleA fromAttrToStr "colwidth" as
	  , colspecColsep = possibleA fromAttrToStr "colsep" as
	  , colspecRowsep = possibleA fromAttrToStr "rowsep" as
	  , colspecAlign = possibleA fromAttrToTyp "align" as
	  , colspecChar = possibleA fromAttrToStr "char" as
	  , colspecCharoff = possibleA fromAttrToStr "charoff" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "colnum" (colspecColnum v)
	, maybeToAttr toAttrFrStr "colname" (colspecColname v)
	, maybeToAttr toAttrFrStr "colwidth" (colspecColwidth v)
	, maybeToAttr toAttrFrStr "colsep" (colspecColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (colspecRowsep v)
	, maybeToAttr toAttrFrTyp "align" (colspecAlign v)
	, maybeToAttr toAttrFrStr "char" (colspecChar v)
	, maybeToAttr toAttrFrStr "charoff" (colspecCharoff v)
	]
instance XmlAttrType Colspec_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Colspec_Align_Left
	    translate "right" = Just Colspec_Align_Right
	    translate "center" = Just Colspec_Align_Center
	    translate "justify" = Just Colspec_Align_Justify
	    translate "char" = Just Colspec_Align_Char
	    translate _ = Nothing
    toAttrFrTyp n Colspec_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Colspec_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Colspec_Align_Center = Just (n, str2attr "center")
    toAttrFrTyp n Colspec_Align_Justify = Just (n, str2attr "justify")
    toAttrFrTyp n Colspec_Align_Char = Just (n, str2attr "char")
instance XmlContent Spanspec where
    fromElem (CElem (Elem "spanspec" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "spanspec" (toAttrs as) [])]
instance XmlAttributes Spanspec where
    fromAttrs as =
	Spanspec
	  { spanspecNamest = definiteA fromAttrToStr "spanspec" "namest" as
	  , spanspecNameend = definiteA fromAttrToStr "spanspec" "nameend" as
	  , spanspecSpanname = definiteA fromAttrToStr "spanspec" "spanname" as
	  , spanspecColsep = possibleA fromAttrToStr "colsep" as
	  , spanspecRowsep = possibleA fromAttrToStr "rowsep" as
	  , spanspecAlign = possibleA fromAttrToTyp "align" as
	  , spanspecChar = possibleA fromAttrToStr "char" as
	  , spanspecCharoff = possibleA fromAttrToStr "charoff" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "namest" (spanspecNamest v)
	, toAttrFrStr "nameend" (spanspecNameend v)
	, toAttrFrStr "spanname" (spanspecSpanname v)
	, maybeToAttr toAttrFrStr "colsep" (spanspecColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (spanspecRowsep v)
	, maybeToAttr toAttrFrTyp "align" (spanspecAlign v)
	, maybeToAttr toAttrFrStr "char" (spanspecChar v)
	, maybeToAttr toAttrFrStr "charoff" (spanspecCharoff v)
	]
instance XmlAttrType Spanspec_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Spanspec_Align_Left
	    translate "right" = Just Spanspec_Align_Right
	    translate "center" = Just Spanspec_Align_Center
	    translate "justify" = Just Spanspec_Align_Justify
	    translate "char" = Just Spanspec_Align_Char
	    translate _ = Nothing
    toAttrFrTyp n Spanspec_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Spanspec_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Spanspec_Align_Center = Just (n, str2attr "center")
    toAttrFrTyp n Spanspec_Align_Justify = Just (n, str2attr "justify")
    toAttrFrTyp n Spanspec_Align_Char = Just (n, str2attr "char")
instance XmlContent Thead where
    fromElem (CElem (Elem "thead" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Thead (fromAttrs as) a b), rest))
	   (definite fromElem "row+" "thead" ca))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Thead as a b) =
	[CElem (Elem "thead" (toAttrs as) (concatMap toElem a
					   ++ toElem b))]
instance XmlAttributes Thead_Attrs where
    fromAttrs as =
	Thead_Attrs
	  { theadValign = possibleA fromAttrToTyp "valign" as
	  , theadId = possibleA fromAttrToStr "id" as
	  , theadLang = possibleA fromAttrToStr "lang" as
	  , theadRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , theadRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "valign" (theadValign v)
	, maybeToAttr toAttrFrStr "id" (theadId v)
	, maybeToAttr toAttrFrStr "lang" (theadLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (theadRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (theadRole v)
	]
instance XmlAttrType Thead_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Thead_Valign_Top
	    translate "middle" = Just Thead_Valign_Middle
	    translate "bottom" = Just Thead_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Thead_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Thead_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Thead_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Thead_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Thead_Revisionflag_Changed
	    translate "added" = Just Thead_Revisionflag_Added
	    translate "deleted" = Just Thead_Revisionflag_Deleted
	    translate "off" = Just Thead_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Thead_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Thead_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Thead_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Thead_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Tfoot where
    fromElem (CElem (Elem "tfoot" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Tfoot (fromAttrs as) a b), rest))
	   (definite fromElem "row+" "tfoot" ca))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Tfoot as a b) =
	[CElem (Elem "tfoot" (toAttrs as) (concatMap toElem a
					   ++ toElem b))]
instance XmlAttributes Tfoot_Attrs where
    fromAttrs as =
	Tfoot_Attrs
	  { tfootValign = possibleA fromAttrToTyp "valign" as
	  , tfootId = possibleA fromAttrToStr "id" as
	  , tfootLang = possibleA fromAttrToStr "lang" as
	  , tfootRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , tfootRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "valign" (tfootValign v)
	, maybeToAttr toAttrFrStr "id" (tfootId v)
	, maybeToAttr toAttrFrStr "lang" (tfootLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (tfootRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (tfootRole v)
	]
instance XmlAttrType Tfoot_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Tfoot_Valign_Top
	    translate "middle" = Just Tfoot_Valign_Middle
	    translate "bottom" = Just Tfoot_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Tfoot_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Tfoot_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Tfoot_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Tfoot_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Tfoot_Revisionflag_Changed
	    translate "added" = Just Tfoot_Revisionflag_Added
	    translate "deleted" = Just Tfoot_Revisionflag_Deleted
	    translate "off" = Just Tfoot_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Tfoot_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Tfoot_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Tfoot_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Tfoot_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Tbody where
    fromElem (CElem (Elem "tbody" as c0):rest) =
	(\(a,ca)->
	   (Just (Tbody (fromAttrs as) a), rest))
	(definite fromElem "row+" "tbody" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Tbody as a) =
	[CElem (Elem "tbody" (toAttrs as) (toElem a))]
instance XmlAttributes Tbody_Attrs where
    fromAttrs as =
	Tbody_Attrs
	  { tbodyValign = possibleA fromAttrToTyp "valign" as
	  , tbodyId = possibleA fromAttrToStr "id" as
	  , tbodyLang = possibleA fromAttrToStr "lang" as
	  , tbodyRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , tbodyRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "valign" (tbodyValign v)
	, maybeToAttr toAttrFrStr "id" (tbodyId v)
	, maybeToAttr toAttrFrStr "lang" (tbodyLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (tbodyRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (tbodyRole v)
	]
instance XmlAttrType Tbody_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Tbody_Valign_Top
	    translate "middle" = Just Tbody_Valign_Middle
	    translate "bottom" = Just Tbody_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Tbody_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Tbody_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Tbody_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Tbody_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Tbody_Revisionflag_Changed
	    translate "added" = Just Tbody_Revisionflag_Added
	    translate "deleted" = Just Tbody_Revisionflag_Deleted
	    translate "off" = Just Tbody_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Tbody_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Tbody_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Tbody_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Tbody_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Row where
    fromElem (CElem (Elem "row" as c0):rest) =
	(\(a,ca)->
	   (Just (Row (fromAttrs as) a), rest))
	(definite fromElem "(entry|entrytbl)+" "row" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Row as a) =
	[CElem (Elem "row" (toAttrs as) (toElem a))]
instance XmlAttributes Row_Attrs where
    fromAttrs as =
	Row_Attrs
	  { rowRowsep = possibleA fromAttrToStr "rowsep" as
	  , rowValign = possibleA fromAttrToTyp "valign" as
	  , rowId = possibleA fromAttrToStr "id" as
	  , rowLang = possibleA fromAttrToStr "lang" as
	  , rowRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , rowRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "rowsep" (rowRowsep v)
	, maybeToAttr toAttrFrTyp "valign" (rowValign v)
	, maybeToAttr toAttrFrStr "id" (rowId v)
	, maybeToAttr toAttrFrStr "lang" (rowLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (rowRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (rowRole v)
	]
instance XmlAttrType Row_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Row_Valign_Top
	    translate "middle" = Just Row_Valign_Middle
	    translate "bottom" = Just Row_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Row_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Row_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Row_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Row_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Row_Revisionflag_Changed
	    translate "added" = Just Row_Revisionflag_Added
	    translate "deleted" = Just Row_Revisionflag_Deleted
	    translate "off" = Just Row_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Row_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Row_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Row_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Row_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Entrytbl where
    fromElem (CElem (Elem "entrytbl" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (Just (Entrytbl (fromAttrs as) a b c d), rest))
		 (definite fromElem "<tbody>" "entrytbl" cc))
	      (fromElem cb))
	   (many fromElem ca))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Entrytbl as a b c d) =
	[CElem (Elem "entrytbl" (toAttrs as) (concatMap toElem a
					      ++ concatMap toElem b ++
					      maybe [] toElem c ++ toElem d))]
instance XmlAttributes Entrytbl_Attrs where
    fromAttrs as =
	Entrytbl_Attrs
	  { entrytblCols = definiteA fromAttrToStr "entrytbl" "cols" as
	  , entrytblTgroupstyle = possibleA fromAttrToStr "tgroupstyle" as
	  , entrytblColname = possibleA fromAttrToStr "colname" as
	  , entrytblSpanname = possibleA fromAttrToStr "spanname" as
	  , entrytblNamest = possibleA fromAttrToStr "namest" as
	  , entrytblNameend = possibleA fromAttrToStr "nameend" as
	  , entrytblColsep = possibleA fromAttrToStr "colsep" as
	  , entrytblRowsep = possibleA fromAttrToStr "rowsep" as
	  , entrytblAlign = possibleA fromAttrToTyp "align" as
	  , entrytblChar = possibleA fromAttrToStr "char" as
	  , entrytblCharoff = possibleA fromAttrToStr "charoff" as
	  , entrytblId = possibleA fromAttrToStr "id" as
	  , entrytblLang = possibleA fromAttrToStr "lang" as
	  , entrytblRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , entrytblRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "cols" (entrytblCols v)
	, maybeToAttr toAttrFrStr "tgroupstyle" (entrytblTgroupstyle v)
	, maybeToAttr toAttrFrStr "colname" (entrytblColname v)
	, maybeToAttr toAttrFrStr "spanname" (entrytblSpanname v)
	, maybeToAttr toAttrFrStr "namest" (entrytblNamest v)
	, maybeToAttr toAttrFrStr "nameend" (entrytblNameend v)
	, maybeToAttr toAttrFrStr "colsep" (entrytblColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (entrytblRowsep v)
	, maybeToAttr toAttrFrTyp "align" (entrytblAlign v)
	, maybeToAttr toAttrFrStr "char" (entrytblChar v)
	, maybeToAttr toAttrFrStr "charoff" (entrytblCharoff v)
	, maybeToAttr toAttrFrStr "id" (entrytblId v)
	, maybeToAttr toAttrFrStr "lang" (entrytblLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (entrytblRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (entrytblRole v)
	]
instance XmlAttrType Entrytbl_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Entrytbl_Align_Left
	    translate "right" = Just Entrytbl_Align_Right
	    translate "center" = Just Entrytbl_Align_Center
	    translate "justify" = Just Entrytbl_Align_Justify
	    translate "char" = Just Entrytbl_Align_Char
	    translate _ = Nothing
    toAttrFrTyp n Entrytbl_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Entrytbl_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Entrytbl_Align_Center = Just (n, str2attr "center")
    toAttrFrTyp n Entrytbl_Align_Justify = Just (n, str2attr "justify")
    toAttrFrTyp n Entrytbl_Align_Char = Just (n, str2attr "char")
instance XmlAttrType Entrytbl_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Entrytbl_Revisionflag_Changed
	    translate "added" = Just Entrytbl_Revisionflag_Added
	    translate "deleted" = Just Entrytbl_Revisionflag_Deleted
	    translate "off" = Just Entrytbl_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Entrytbl_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Entrytbl_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Entrytbl_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Entrytbl_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Entry where
    fromElem (CElem (Elem "entry" as c0):rest) =
	(\(a,ca)->
	   (Just (Entry (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Entry as a) =
	[CElem (Elem "entry" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Entry_Attrs where
    fromAttrs as =
	Entry_Attrs
	  { entryColname = possibleA fromAttrToStr "colname" as
	  , entryNamest = possibleA fromAttrToStr "namest" as
	  , entryNameend = possibleA fromAttrToStr "nameend" as
	  , entrySpanname = possibleA fromAttrToStr "spanname" as
	  , entryMorerows = possibleA fromAttrToStr "morerows" as
	  , entryColsep = possibleA fromAttrToStr "colsep" as
	  , entryRowsep = possibleA fromAttrToStr "rowsep" as
	  , entryAlign = possibleA fromAttrToTyp "align" as
	  , entryChar = possibleA fromAttrToStr "char" as
	  , entryCharoff = possibleA fromAttrToStr "charoff" as
	  , entryRotate = possibleA fromAttrToStr "rotate" as
	  , entryValign = possibleA fromAttrToTyp "valign" as
	  , entryId = possibleA fromAttrToStr "id" as
	  , entryLang = possibleA fromAttrToStr "lang" as
	  , entryRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , entryRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "colname" (entryColname v)
	, maybeToAttr toAttrFrStr "namest" (entryNamest v)
	, maybeToAttr toAttrFrStr "nameend" (entryNameend v)
	, maybeToAttr toAttrFrStr "spanname" (entrySpanname v)
	, maybeToAttr toAttrFrStr "morerows" (entryMorerows v)
	, maybeToAttr toAttrFrStr "colsep" (entryColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (entryRowsep v)
	, maybeToAttr toAttrFrTyp "align" (entryAlign v)
	, maybeToAttr toAttrFrStr "char" (entryChar v)
	, maybeToAttr toAttrFrStr "charoff" (entryCharoff v)
	, maybeToAttr toAttrFrStr "rotate" (entryRotate v)
	, maybeToAttr toAttrFrTyp "valign" (entryValign v)
	, maybeToAttr toAttrFrStr "id" (entryId v)
	, maybeToAttr toAttrFrStr "lang" (entryLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (entryRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (entryRole v)
	]
instance XmlContent Entry_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Entry_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Entry_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Entry_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Entry_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Entry_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Entry_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Entry_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Entry_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Entry_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Entry_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Entry_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Entry_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Entry_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Entry_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Entry_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Entry_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Entry_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Entry_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Entry_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Entry_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Entry_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Entry_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Entry_Inlinemediaobject a), rest)
																							(_,_) ->
																								case (fromElem c0) of
																								(Just a,rest) -> (Just (Entry_Itemizedlist a), rest)
																								(_,_) ->
																									case (fromElem c0) of
																									(Just a,rest) -> (Just (Entry_Orderedlist a), rest)
																									(_,_) ->
																										case (fromElem c0) of
																										(Just a,rest) -> (Just (Entry_Variablelist a), rest)
																										(_,_) ->
																											case (fromElem c0) of
																											(Just a,rest) -> (Just (Entry_Note a), rest)
																											(_,_) ->
																												case (fromElem c0) of
																												(Just a,rest) -> (Just (Entry_Literallayout a), rest)
																												(_,_) ->
																													case (fromElem c0) of
																													(Just a,rest) -> (Just (Entry_Programlisting a), rest)
																													(_,_) ->
																														case (fromElem c0) of
																														(Just a,rest) -> (Just (Entry_Para a), rest)
																														(_,_) ->
																															case (fromElem c0) of
																															(Just a,rest) -> (Just (Entry_Mediaobject a), rest)
																															(_,_) ->
																															    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Entry_Str a) = toText a
    toElem (Entry_Footnoteref a) = toElem a
    toElem (Entry_Xref a) = toElem a
    toElem (Entry_Abbrev a) = toElem a
    toElem (Entry_Acronym a) = toElem a
    toElem (Entry_Citetitle a) = toElem a
    toElem (Entry_Emphasis a) = toElem a
    toElem (Entry_Footnote a) = toElem a
    toElem (Entry_Phrase a) = toElem a
    toElem (Entry_Quote a) = toElem a
    toElem (Entry_Trademark a) = toElem a
    toElem (Entry_Link a) = toElem a
    toElem (Entry_Ulink a) = toElem a
    toElem (Entry_Command a) = toElem a
    toElem (Entry_Computeroutput a) = toElem a
    toElem (Entry_Email a) = toElem a
    toElem (Entry_Filename a) = toElem a
    toElem (Entry_Literal a) = toElem a
    toElem (Entry_Option a) = toElem a
    toElem (Entry_Replaceable a) = toElem a
    toElem (Entry_Systemitem a) = toElem a
    toElem (Entry_Userinput a) = toElem a
    toElem (Entry_Inlinemediaobject a) = toElem a
    toElem (Entry_Itemizedlist a) = toElem a
    toElem (Entry_Orderedlist a) = toElem a
    toElem (Entry_Variablelist a) = toElem a
    toElem (Entry_Note a) = toElem a
    toElem (Entry_Literallayout a) = toElem a
    toElem (Entry_Programlisting a) = toElem a
    toElem (Entry_Para a) = toElem a
    toElem (Entry_Mediaobject a) = toElem a
instance XmlAttrType Entry_Align where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "left" = Just Entry_Align_Left
	    translate "right" = Just Entry_Align_Right
	    translate "center" = Just Entry_Align_Center
	    translate "justify" = Just Entry_Align_Justify
	    translate "char" = Just Entry_Align_Char
	    translate _ = Nothing
    toAttrFrTyp n Entry_Align_Left = Just (n, str2attr "left")
    toAttrFrTyp n Entry_Align_Right = Just (n, str2attr "right")
    toAttrFrTyp n Entry_Align_Center = Just (n, str2attr "center")
    toAttrFrTyp n Entry_Align_Justify = Just (n, str2attr "justify")
    toAttrFrTyp n Entry_Align_Char = Just (n, str2attr "char")
instance XmlAttrType Entry_Valign where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Entry_Valign_Top
	    translate "middle" = Just Entry_Valign_Middle
	    translate "bottom" = Just Entry_Valign_Bottom
	    translate _ = Nothing
    toAttrFrTyp n Entry_Valign_Top = Just (n, str2attr "top")
    toAttrFrTyp n Entry_Valign_Middle = Just (n, str2attr "middle")
    toAttrFrTyp n Entry_Valign_Bottom = Just (n, str2attr "bottom")
instance XmlAttrType Entry_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Entry_Revisionflag_Changed
	    translate "added" = Just Entry_Revisionflag_Added
	    translate "deleted" = Just Entry_Revisionflag_Deleted
	    translate "off" = Just Entry_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Entry_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Entry_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Entry_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Entry_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Informaltable where
    fromElem (CElem (Elem "informaltable" as c0):rest) =
	case (fromElem c0) of
	(Just a,_) -> (Just (InformaltableMediaobject (fromAttrs as) a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,_) -> (Just (InformaltableTgroup (fromAttrs as) a), rest)
		(_,_) ->
		    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (InformaltableMediaobject as a) = [CElem (Elem "informaltable" (toAttrs as) (toElem a) )]
    toElem (InformaltableTgroup as a) = [CElem (Elem "informaltable" (toAttrs as) (toElem a) )]
instance XmlAttributes Informaltable_Attrs where
    fromAttrs as =
	Informaltable_Attrs
	  { informaltableFrame = possibleA fromAttrToTyp "frame" as
	  , informaltableColsep = possibleA fromAttrToStr "colsep" as
	  , informaltableRowsep = possibleA fromAttrToStr "rowsep" as
	  , informaltableLabel = possibleA fromAttrToStr "label" as
	  , informaltableId = possibleA fromAttrToStr "id" as
	  , informaltableLang = possibleA fromAttrToStr "lang" as
	  , informaltableRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , informaltableRole = possibleA fromAttrToStr "role" as
	  , informaltableTabstyle = possibleA fromAttrToStr "tabstyle" as
	  , informaltableTocentry = possibleA fromAttrToStr "tocentry" as
	  , informaltableShortentry = possibleA fromAttrToStr "shortentry" as
	  , informaltableOrient = possibleA fromAttrToTyp "orient" as
	  , informaltablePgwide = possibleA fromAttrToStr "pgwide" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "frame" (informaltableFrame v)
	, maybeToAttr toAttrFrStr "colsep" (informaltableColsep v)
	, maybeToAttr toAttrFrStr "rowsep" (informaltableRowsep v)
	, maybeToAttr toAttrFrStr "label" (informaltableLabel v)
	, maybeToAttr toAttrFrStr "id" (informaltableId v)
	, maybeToAttr toAttrFrStr "lang" (informaltableLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (informaltableRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (informaltableRole v)
	, maybeToAttr toAttrFrStr "tabstyle" (informaltableTabstyle v)
	, maybeToAttr toAttrFrStr "tocentry" (informaltableTocentry v)
	, maybeToAttr toAttrFrStr "shortentry" (informaltableShortentry v)
	, maybeToAttr toAttrFrTyp "orient" (informaltableOrient v)
	, maybeToAttr toAttrFrStr "pgwide" (informaltablePgwide v)
	]
instance XmlAttrType Informaltable_Frame where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "top" = Just Informaltable_Frame_Top
	    translate "bottom" = Just Informaltable_Frame_Bottom
	    translate "topbot" = Just Informaltable_Frame_Topbot
	    translate "all" = Just Informaltable_Frame_All
	    translate "sides" = Just Informaltable_Frame_Sides
	    translate "none" = Just Informaltable_Frame_None
	    translate _ = Nothing
    toAttrFrTyp n Informaltable_Frame_Top = Just (n, str2attr "top")
    toAttrFrTyp n Informaltable_Frame_Bottom = Just (n, str2attr "bottom")
    toAttrFrTyp n Informaltable_Frame_Topbot = Just (n, str2attr "topbot")
    toAttrFrTyp n Informaltable_Frame_All = Just (n, str2attr "all")
    toAttrFrTyp n Informaltable_Frame_Sides = Just (n, str2attr "sides")
    toAttrFrTyp n Informaltable_Frame_None = Just (n, str2attr "none")
instance XmlAttrType Informaltable_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Informaltable_Revisionflag_Changed
	    translate "added" = Just Informaltable_Revisionflag_Added
	    translate "deleted" = Just Informaltable_Revisionflag_Deleted
	    translate "off" = Just Informaltable_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Informaltable_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Informaltable_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Informaltable_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Informaltable_Revisionflag_Off = Just (n, str2attr "off")
instance XmlAttrType Informaltable_Orient where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "port" = Just Informaltable_Orient_Port
	    translate "land" = Just Informaltable_Orient_Land
	    translate _ = Nothing
    toAttrFrTyp n Informaltable_Orient_Port = Just (n, str2attr "port")
    toAttrFrTyp n Informaltable_Orient_Land = Just (n, str2attr "land")
instance XmlContent Affiliation where
    fromElem (CElem (Elem "affiliation" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Affiliation (fromAttrs as) a b), rest))
	   (fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Affiliation as a b) =
	[CElem (Elem "affiliation" (toAttrs as) (maybe [] toElem a
						 ++ maybe [] toElem b))]
instance XmlAttributes Affiliation_Attrs where
    fromAttrs as =
	Affiliation_Attrs
	  { affiliationId = possibleA fromAttrToStr "id" as
	  , affiliationLang = possibleA fromAttrToStr "lang" as
	  , affiliationRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , affiliationRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (affiliationId v)
	, maybeToAttr toAttrFrStr "lang" (affiliationLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (affiliationRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (affiliationRole v)
	]
instance XmlAttrType Affiliation_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Affiliation_Revisionflag_Changed
	    translate "added" = Just Affiliation_Revisionflag_Added
	    translate "deleted" = Just Affiliation_Revisionflag_Deleted
	    translate "off" = Just Affiliation_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Affiliation_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Affiliation_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Affiliation_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Affiliation_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Jobtitle where
    fromElem (CElem (Elem "jobtitle" as c0):rest) =
	(\(a,ca)->
	   (Just (Jobtitle (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Jobtitle as a) =
	[CElem (Elem "jobtitle" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Jobtitle_Attrs where
    fromAttrs as =
	Jobtitle_Attrs
	  { jobtitleId = possibleA fromAttrToStr "id" as
	  , jobtitleLang = possibleA fromAttrToStr "lang" as
	  , jobtitleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , jobtitleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (jobtitleId v)
	, maybeToAttr toAttrFrStr "lang" (jobtitleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (jobtitleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (jobtitleRole v)
	]
instance XmlContent Jobtitle_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Jobtitle_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Jobtitle_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Jobtitle_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Jobtitle_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Jobtitle_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Jobtitle_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Jobtitle_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Jobtitle_Str a) = toText a
    toElem (Jobtitle_Link a) = toElem a
    toElem (Jobtitle_Ulink a) = toElem a
    toElem (Jobtitle_Emphasis a) = toElem a
    toElem (Jobtitle_Trademark a) = toElem a
    toElem (Jobtitle_Replaceable a) = toElem a
    toElem (Jobtitle_Inlinemediaobject a) = toElem a
instance XmlAttrType Jobtitle_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Jobtitle_Revisionflag_Changed
	    translate "added" = Just Jobtitle_Revisionflag_Added
	    translate "deleted" = Just Jobtitle_Revisionflag_Deleted
	    translate "off" = Just Jobtitle_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Jobtitle_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Jobtitle_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Jobtitle_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Jobtitle_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Author where
    fromElem (CElem (Elem "author" as c0):rest) =
	(\(a,ca)->
	   (Just (Author (fromAttrs as) a), rest))
	(definite fromElem "(honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "author" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Author as a) =
	[CElem (Elem "author" (toAttrs as) (toElem a))]
instance XmlAttributes Author_Attrs where
    fromAttrs as =
	Author_Attrs
	  { authorId = possibleA fromAttrToStr "id" as
	  , authorLang = possibleA fromAttrToStr "lang" as
	  , authorRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , authorRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (authorId v)
	, maybeToAttr toAttrFrStr "lang" (authorLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (authorRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (authorRole v)
	]
instance XmlAttrType Author_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Author_Revisionflag_Changed
	    translate "added" = Just Author_Revisionflag_Added
	    translate "deleted" = Just Author_Revisionflag_Deleted
	    translate "off" = Just Author_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Author_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Author_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Author_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Author_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Authorgroup where
    fromElem (CElem (Elem "authorgroup" as c0):rest) =
	(\(a,ca)->
	   (Just (Authorgroup (fromAttrs as) a), rest))
	(definite fromElem "(author|editor|corpauthor|othercredit)+" "authorgroup" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Authorgroup as a) =
	[CElem (Elem "authorgroup" (toAttrs as) (toElem a))]
instance XmlAttributes Authorgroup_Attrs where
    fromAttrs as =
	Authorgroup_Attrs
	  { authorgroupId = possibleA fromAttrToStr "id" as
	  , authorgroupLang = possibleA fromAttrToStr "lang" as
	  , authorgroupRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , authorgroupRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (authorgroupId v)
	, maybeToAttr toAttrFrStr "lang" (authorgroupLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (authorgroupRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (authorgroupRole v)
	]
instance XmlAttrType Authorgroup_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Authorgroup_Revisionflag_Changed
	    translate "added" = Just Authorgroup_Revisionflag_Added
	    translate "deleted" = Just Authorgroup_Revisionflag_Deleted
	    translate "off" = Just Authorgroup_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Authorgroup_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Authorgroup_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Authorgroup_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Authorgroup_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Authorinitials where
    fromElem (CElem (Elem "authorinitials" as c0):rest) =
	(\(a,ca)->
	   (Just (Authorinitials (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Authorinitials as a) =
	[CElem (Elem "authorinitials" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Authorinitials_Attrs where
    fromAttrs as =
	Authorinitials_Attrs
	  { authorinitialsId = possibleA fromAttrToStr "id" as
	  , authorinitialsLang = possibleA fromAttrToStr "lang" as
	  , authorinitialsRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , authorinitialsRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (authorinitialsId v)
	, maybeToAttr toAttrFrStr "lang" (authorinitialsLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (authorinitialsRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (authorinitialsRole v)
	]
instance XmlContent Authorinitials_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Authorinitials_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Authorinitials_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Authorinitials_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Authorinitials_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Authorinitials_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Authorinitials_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Authorinitials_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Authorinitials_Str a) = toText a
    toElem (Authorinitials_Link a) = toElem a
    toElem (Authorinitials_Ulink a) = toElem a
    toElem (Authorinitials_Emphasis a) = toElem a
    toElem (Authorinitials_Trademark a) = toElem a
    toElem (Authorinitials_Replaceable a) = toElem a
    toElem (Authorinitials_Inlinemediaobject a) = toElem a
instance XmlAttrType Authorinitials_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Authorinitials_Revisionflag_Changed
	    translate "added" = Just Authorinitials_Revisionflag_Added
	    translate "deleted" = Just Authorinitials_Revisionflag_Deleted
	    translate "off" = Just Authorinitials_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Authorinitials_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Authorinitials_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Authorinitials_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Authorinitials_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Copyright where
    fromElem (CElem (Elem "copyright" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Copyright (fromAttrs as) a b), rest))
	   (many fromElem ca))
	(definite fromElem "year+" "copyright" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Copyright as a b) =
	[CElem (Elem "copyright" (toAttrs as) (toElem a ++
					       concatMap toElem b))]
instance XmlAttributes Copyright_Attrs where
    fromAttrs as =
	Copyright_Attrs
	  { copyrightId = possibleA fromAttrToStr "id" as
	  , copyrightLang = possibleA fromAttrToStr "lang" as
	  , copyrightRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , copyrightRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (copyrightId v)
	, maybeToAttr toAttrFrStr "lang" (copyrightLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (copyrightRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (copyrightRole v)
	]
instance XmlAttrType Copyright_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Copyright_Revisionflag_Changed
	    translate "added" = Just Copyright_Revisionflag_Added
	    translate "deleted" = Just Copyright_Revisionflag_Deleted
	    translate "off" = Just Copyright_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Copyright_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Copyright_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Copyright_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Copyright_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Year where
    fromElem (CElem (Elem "year" as c0):rest) =
	(\(a,ca)->
	   (Just (Year (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Year as a) =
	[CElem (Elem "year" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Year_Attrs where
    fromAttrs as =
	Year_Attrs
	  { yearId = possibleA fromAttrToStr "id" as
	  , yearLang = possibleA fromAttrToStr "lang" as
	  , yearRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , yearRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (yearId v)
	, maybeToAttr toAttrFrStr "lang" (yearLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (yearRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (yearRole v)
	]
instance XmlContent Year_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Year_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Year_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Year_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Year_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Year_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Year_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Year_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Year_Str a) = toText a
    toElem (Year_Link a) = toElem a
    toElem (Year_Ulink a) = toElem a
    toElem (Year_Emphasis a) = toElem a
    toElem (Year_Trademark a) = toElem a
    toElem (Year_Replaceable a) = toElem a
    toElem (Year_Inlinemediaobject a) = toElem a
instance XmlAttrType Year_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Year_Revisionflag_Changed
	    translate "added" = Just Year_Revisionflag_Added
	    translate "deleted" = Just Year_Revisionflag_Deleted
	    translate "off" = Just Year_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Year_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Year_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Year_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Year_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Holder where
    fromElem (CElem (Elem "holder" as c0):rest) =
	(\(a,ca)->
	   (Just (Holder (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Holder as a) =
	[CElem (Elem "holder" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Holder_Attrs where
    fromAttrs as =
	Holder_Attrs
	  { holderId = possibleA fromAttrToStr "id" as
	  , holderLang = possibleA fromAttrToStr "lang" as
	  , holderRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , holderRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (holderId v)
	, maybeToAttr toAttrFrStr "lang" (holderLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (holderRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (holderRole v)
	]
instance XmlContent Holder_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Holder_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Holder_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Holder_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Holder_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Holder_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Holder_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Holder_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Holder_Str a) = toText a
    toElem (Holder_Link a) = toElem a
    toElem (Holder_Ulink a) = toElem a
    toElem (Holder_Emphasis a) = toElem a
    toElem (Holder_Trademark a) = toElem a
    toElem (Holder_Replaceable a) = toElem a
    toElem (Holder_Inlinemediaobject a) = toElem a
instance XmlAttrType Holder_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Holder_Revisionflag_Changed
	    translate "added" = Just Holder_Revisionflag_Added
	    translate "deleted" = Just Holder_Revisionflag_Deleted
	    translate "off" = Just Holder_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Holder_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Holder_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Holder_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Holder_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Corpauthor where
    fromElem (CElem (Elem "corpauthor" as c0):rest) =
	(\(a,ca)->
	   (Just (Corpauthor (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Corpauthor as a) =
	[CElem (Elem "corpauthor" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Corpauthor_Attrs where
    fromAttrs as =
	Corpauthor_Attrs
	  { corpauthorId = possibleA fromAttrToStr "id" as
	  , corpauthorLang = possibleA fromAttrToStr "lang" as
	  , corpauthorRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , corpauthorRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (corpauthorId v)
	, maybeToAttr toAttrFrStr "lang" (corpauthorLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (corpauthorRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (corpauthorRole v)
	]
instance XmlContent Corpauthor_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Corpauthor_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Corpauthor_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Corpauthor_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Corpauthor_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Corpauthor_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Corpauthor_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Corpauthor_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Corpauthor_Str a) = toText a
    toElem (Corpauthor_Link a) = toElem a
    toElem (Corpauthor_Ulink a) = toElem a
    toElem (Corpauthor_Emphasis a) = toElem a
    toElem (Corpauthor_Trademark a) = toElem a
    toElem (Corpauthor_Replaceable a) = toElem a
    toElem (Corpauthor_Inlinemediaobject a) = toElem a
instance XmlAttrType Corpauthor_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Corpauthor_Revisionflag_Changed
	    translate "added" = Just Corpauthor_Revisionflag_Added
	    translate "deleted" = Just Corpauthor_Revisionflag_Deleted
	    translate "off" = Just Corpauthor_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Corpauthor_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Corpauthor_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Corpauthor_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Corpauthor_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Date where
    fromElem (CElem (Elem "date" as c0):rest) =
	(\(a,ca)->
	   (Just (Date (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Date as a) =
	[CElem (Elem "date" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Date_Attrs where
    fromAttrs as =
	Date_Attrs
	  { dateId = possibleA fromAttrToStr "id" as
	  , dateLang = possibleA fromAttrToStr "lang" as
	  , dateRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , dateRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (dateId v)
	, maybeToAttr toAttrFrStr "lang" (dateLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (dateRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (dateRole v)
	]
instance XmlContent Date_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Date_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Date_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Date_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Date_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Date_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Date_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Date_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Date_Str a) = toText a
    toElem (Date_Link a) = toElem a
    toElem (Date_Ulink a) = toElem a
    toElem (Date_Emphasis a) = toElem a
    toElem (Date_Trademark a) = toElem a
    toElem (Date_Replaceable a) = toElem a
    toElem (Date_Inlinemediaobject a) = toElem a
instance XmlAttrType Date_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Date_Revisionflag_Changed
	    translate "added" = Just Date_Revisionflag_Added
	    translate "deleted" = Just Date_Revisionflag_Deleted
	    translate "off" = Just Date_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Date_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Date_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Date_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Date_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Edition where
    fromElem (CElem (Elem "edition" as c0):rest) =
	(\(a,ca)->
	   (Just (Edition (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Edition as a) =
	[CElem (Elem "edition" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Edition_Attrs where
    fromAttrs as =
	Edition_Attrs
	  { editionId = possibleA fromAttrToStr "id" as
	  , editionLang = possibleA fromAttrToStr "lang" as
	  , editionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , editionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (editionId v)
	, maybeToAttr toAttrFrStr "lang" (editionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (editionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (editionRole v)
	]
instance XmlContent Edition_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Edition_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Edition_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Edition_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Edition_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Edition_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Edition_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Edition_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Edition_Str a) = toText a
    toElem (Edition_Link a) = toElem a
    toElem (Edition_Ulink a) = toElem a
    toElem (Edition_Emphasis a) = toElem a
    toElem (Edition_Trademark a) = toElem a
    toElem (Edition_Replaceable a) = toElem a
    toElem (Edition_Inlinemediaobject a) = toElem a
instance XmlAttrType Edition_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Edition_Revisionflag_Changed
	    translate "added" = Just Edition_Revisionflag_Added
	    translate "deleted" = Just Edition_Revisionflag_Deleted
	    translate "off" = Just Edition_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Edition_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Edition_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Edition_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Edition_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Editor where
    fromElem (CElem (Elem "editor" as c0):rest) =
	(\(a,ca)->
	   (Just (Editor (fromAttrs as) a), rest))
	(definite fromElem "(honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "editor" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Editor as a) =
	[CElem (Elem "editor" (toAttrs as) (toElem a))]
instance XmlAttributes Editor_Attrs where
    fromAttrs as =
	Editor_Attrs
	  { editorId = possibleA fromAttrToStr "id" as
	  , editorLang = possibleA fromAttrToStr "lang" as
	  , editorRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , editorRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (editorId v)
	, maybeToAttr toAttrFrStr "lang" (editorLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (editorRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (editorRole v)
	]
instance XmlAttrType Editor_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Editor_Revisionflag_Changed
	    translate "added" = Just Editor_Revisionflag_Added
	    translate "deleted" = Just Editor_Revisionflag_Deleted
	    translate "off" = Just Editor_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Editor_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Editor_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Editor_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Editor_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Issuenum where
    fromElem (CElem (Elem "issuenum" as c0):rest) =
	(\(a,ca)->
	   (Just (Issuenum (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Issuenum as a) =
	[CElem (Elem "issuenum" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Issuenum_Attrs where
    fromAttrs as =
	Issuenum_Attrs
	  { issuenumId = possibleA fromAttrToStr "id" as
	  , issuenumLang = possibleA fromAttrToStr "lang" as
	  , issuenumRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , issuenumRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (issuenumId v)
	, maybeToAttr toAttrFrStr "lang" (issuenumLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (issuenumRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (issuenumRole v)
	]
instance XmlContent Issuenum_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Issuenum_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Issuenum_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Issuenum_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Issuenum_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Issuenum_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Issuenum_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Issuenum_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Issuenum_Str a) = toText a
    toElem (Issuenum_Link a) = toElem a
    toElem (Issuenum_Ulink a) = toElem a
    toElem (Issuenum_Emphasis a) = toElem a
    toElem (Issuenum_Trademark a) = toElem a
    toElem (Issuenum_Replaceable a) = toElem a
    toElem (Issuenum_Inlinemediaobject a) = toElem a
instance XmlAttrType Issuenum_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Issuenum_Revisionflag_Changed
	    translate "added" = Just Issuenum_Revisionflag_Added
	    translate "deleted" = Just Issuenum_Revisionflag_Deleted
	    translate "off" = Just Issuenum_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Issuenum_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Issuenum_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Issuenum_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Issuenum_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Legalnotice where
    fromElem (CElem (Elem "legalnotice" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Legalnotice (fromAttrs as) a b), rest))
	   (definite fromElem "(itemizedlist|orderedlist|variablelist|note|literallayout|programlisting|para|blockquote)+" "legalnotice" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Legalnotice as a b) =
	[CElem (Elem "legalnotice" (toAttrs as) (maybe [] toElem a
						 ++ toElem b))]
instance XmlAttributes Legalnotice_Attrs where
    fromAttrs as =
	Legalnotice_Attrs
	  { legalnoticeId = possibleA fromAttrToStr "id" as
	  , legalnoticeLang = possibleA fromAttrToStr "lang" as
	  , legalnoticeRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , legalnoticeRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (legalnoticeId v)
	, maybeToAttr toAttrFrStr "lang" (legalnoticeLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (legalnoticeRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (legalnoticeRole v)
	]
instance XmlAttrType Legalnotice_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Legalnotice_Revisionflag_Changed
	    translate "added" = Just Legalnotice_Revisionflag_Added
	    translate "deleted" = Just Legalnotice_Revisionflag_Deleted
	    translate "off" = Just Legalnotice_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Legalnotice_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Legalnotice_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Legalnotice_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Legalnotice_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Orgname where
    fromElem (CElem (Elem "orgname" as c0):rest) =
	(\(a,ca)->
	   (Just (Orgname (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Orgname as a) =
	[CElem (Elem "orgname" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Orgname_Attrs where
    fromAttrs as =
	Orgname_Attrs
	  { orgnameId = possibleA fromAttrToStr "id" as
	  , orgnameLang = possibleA fromAttrToStr "lang" as
	  , orgnameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , orgnameClass = possibleA fromAttrToTyp "class" as
	  , orgnameOtherclass = possibleA fromAttrToStr "otherclass" as
	  , orgnameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (orgnameId v)
	, maybeToAttr toAttrFrStr "lang" (orgnameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (orgnameRevisionflag v)
	, maybeToAttr toAttrFrTyp "class" (orgnameClass v)
	, maybeToAttr toAttrFrStr "otherclass" (orgnameOtherclass v)
	, maybeToAttr toAttrFrStr "role" (orgnameRole v)
	]
instance XmlContent Orgname_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Orgname_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Orgname_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Orgname_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Orgname_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Orgname_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Orgname_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Orgname_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Orgname_Str a) = toText a
    toElem (Orgname_Link a) = toElem a
    toElem (Orgname_Ulink a) = toElem a
    toElem (Orgname_Emphasis a) = toElem a
    toElem (Orgname_Trademark a) = toElem a
    toElem (Orgname_Replaceable a) = toElem a
    toElem (Orgname_Inlinemediaobject a) = toElem a
instance XmlAttrType Orgname_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Orgname_Revisionflag_Changed
	    translate "added" = Just Orgname_Revisionflag_Added
	    translate "deleted" = Just Orgname_Revisionflag_Deleted
	    translate "off" = Just Orgname_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Orgname_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Orgname_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Orgname_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Orgname_Revisionflag_Off = Just (n, str2attr "off")
instance XmlAttrType Orgname_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "corporation" = Just Orgname_Class_Corporation
	    translate "nonprofit" = Just Orgname_Class_Nonprofit
	    translate "consortium" = Just Orgname_Class_Consortium
	    translate "informal" = Just Orgname_Class_Informal
	    translate "other" = Just Orgname_Class_Other
	    translate _ = Nothing
    toAttrFrTyp n Orgname_Class_Corporation = Just (n, str2attr "corporation")
    toAttrFrTyp n Orgname_Class_Nonprofit = Just (n, str2attr "nonprofit")
    toAttrFrTyp n Orgname_Class_Consortium = Just (n, str2attr "consortium")
    toAttrFrTyp n Orgname_Class_Informal = Just (n, str2attr "informal")
    toAttrFrTyp n Orgname_Class_Other = Just (n, str2attr "other")
instance XmlContent Othercredit where
    fromElem (CElem (Elem "othercredit" as c0):rest) =
	(\(a,ca)->
	   (Just (Othercredit (fromAttrs as) a), rest))
	(definite fromElem "(honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "othercredit" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Othercredit as a) =
	[CElem (Elem "othercredit" (toAttrs as) (toElem a))]
instance XmlAttributes Othercredit_Attrs where
    fromAttrs as =
	Othercredit_Attrs
	  { othercreditId = possibleA fromAttrToStr "id" as
	  , othercreditLang = possibleA fromAttrToStr "lang" as
	  , othercreditRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , othercreditRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (othercreditId v)
	, maybeToAttr toAttrFrStr "lang" (othercreditLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (othercreditRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (othercreditRole v)
	]
instance XmlAttrType Othercredit_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Othercredit_Revisionflag_Changed
	    translate "added" = Just Othercredit_Revisionflag_Added
	    translate "deleted" = Just Othercredit_Revisionflag_Deleted
	    translate "off" = Just Othercredit_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Othercredit_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Othercredit_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Othercredit_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Othercredit_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Firstname where
    fromElem (CElem (Elem "firstname" as c0):rest) =
	(\(a,ca)->
	   (Just (Firstname (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Firstname as a) =
	[CElem (Elem "firstname" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Firstname_Attrs where
    fromAttrs as =
	Firstname_Attrs
	  { firstnameId = possibleA fromAttrToStr "id" as
	  , firstnameLang = possibleA fromAttrToStr "lang" as
	  , firstnameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , firstnameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (firstnameId v)
	, maybeToAttr toAttrFrStr "lang" (firstnameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (firstnameRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (firstnameRole v)
	]
instance XmlContent Firstname_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Firstname_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Firstname_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Firstname_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Firstname_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Firstname_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Firstname_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Firstname_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Firstname_Str a) = toText a
    toElem (Firstname_Link a) = toElem a
    toElem (Firstname_Ulink a) = toElem a
    toElem (Firstname_Emphasis a) = toElem a
    toElem (Firstname_Trademark a) = toElem a
    toElem (Firstname_Replaceable a) = toElem a
    toElem (Firstname_Inlinemediaobject a) = toElem a
instance XmlAttrType Firstname_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Firstname_Revisionflag_Changed
	    translate "added" = Just Firstname_Revisionflag_Added
	    translate "deleted" = Just Firstname_Revisionflag_Deleted
	    translate "off" = Just Firstname_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Firstname_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Firstname_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Firstname_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Firstname_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Honorific where
    fromElem (CElem (Elem "honorific" as c0):rest) =
	(\(a,ca)->
	   (Just (Honorific (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Honorific as a) =
	[CElem (Elem "honorific" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Honorific_Attrs where
    fromAttrs as =
	Honorific_Attrs
	  { honorificId = possibleA fromAttrToStr "id" as
	  , honorificLang = possibleA fromAttrToStr "lang" as
	  , honorificRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , honorificRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (honorificId v)
	, maybeToAttr toAttrFrStr "lang" (honorificLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (honorificRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (honorificRole v)
	]
instance XmlContent Honorific_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Honorific_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Honorific_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Honorific_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Honorific_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Honorific_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Honorific_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Honorific_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Honorific_Str a) = toText a
    toElem (Honorific_Link a) = toElem a
    toElem (Honorific_Ulink a) = toElem a
    toElem (Honorific_Emphasis a) = toElem a
    toElem (Honorific_Trademark a) = toElem a
    toElem (Honorific_Replaceable a) = toElem a
    toElem (Honorific_Inlinemediaobject a) = toElem a
instance XmlAttrType Honorific_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Honorific_Revisionflag_Changed
	    translate "added" = Just Honorific_Revisionflag_Added
	    translate "deleted" = Just Honorific_Revisionflag_Deleted
	    translate "off" = Just Honorific_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Honorific_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Honorific_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Honorific_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Honorific_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Lineage where
    fromElem (CElem (Elem "lineage" as c0):rest) =
	(\(a,ca)->
	   (Just (Lineage (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Lineage as a) =
	[CElem (Elem "lineage" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Lineage_Attrs where
    fromAttrs as =
	Lineage_Attrs
	  { lineageId = possibleA fromAttrToStr "id" as
	  , lineageLang = possibleA fromAttrToStr "lang" as
	  , lineageRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , lineageRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (lineageId v)
	, maybeToAttr toAttrFrStr "lang" (lineageLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (lineageRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (lineageRole v)
	]
instance XmlContent Lineage_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Lineage_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Lineage_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Lineage_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Lineage_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Lineage_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Lineage_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Lineage_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Lineage_Str a) = toText a
    toElem (Lineage_Link a) = toElem a
    toElem (Lineage_Ulink a) = toElem a
    toElem (Lineage_Emphasis a) = toElem a
    toElem (Lineage_Trademark a) = toElem a
    toElem (Lineage_Replaceable a) = toElem a
    toElem (Lineage_Inlinemediaobject a) = toElem a
instance XmlAttrType Lineage_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Lineage_Revisionflag_Changed
	    translate "added" = Just Lineage_Revisionflag_Added
	    translate "deleted" = Just Lineage_Revisionflag_Deleted
	    translate "off" = Just Lineage_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Lineage_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Lineage_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Lineage_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Lineage_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Othername where
    fromElem (CElem (Elem "othername" as c0):rest) =
	(\(a,ca)->
	   (Just (Othername (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Othername as a) =
	[CElem (Elem "othername" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Othername_Attrs where
    fromAttrs as =
	Othername_Attrs
	  { othernameId = possibleA fromAttrToStr "id" as
	  , othernameLang = possibleA fromAttrToStr "lang" as
	  , othernameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , othernameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (othernameId v)
	, maybeToAttr toAttrFrStr "lang" (othernameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (othernameRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (othernameRole v)
	]
instance XmlContent Othername_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Othername_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Othername_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Othername_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Othername_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Othername_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Othername_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Othername_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Othername_Str a) = toText a
    toElem (Othername_Link a) = toElem a
    toElem (Othername_Ulink a) = toElem a
    toElem (Othername_Emphasis a) = toElem a
    toElem (Othername_Trademark a) = toElem a
    toElem (Othername_Replaceable a) = toElem a
    toElem (Othername_Inlinemediaobject a) = toElem a
instance XmlAttrType Othername_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Othername_Revisionflag_Changed
	    translate "added" = Just Othername_Revisionflag_Added
	    translate "deleted" = Just Othername_Revisionflag_Deleted
	    translate "off" = Just Othername_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Othername_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Othername_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Othername_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Othername_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Surname where
    fromElem (CElem (Elem "surname" as c0):rest) =
	(\(a,ca)->
	   (Just (Surname (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Surname as a) =
	[CElem (Elem "surname" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Surname_Attrs where
    fromAttrs as =
	Surname_Attrs
	  { surnameId = possibleA fromAttrToStr "id" as
	  , surnameLang = possibleA fromAttrToStr "lang" as
	  , surnameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , surnameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (surnameId v)
	, maybeToAttr toAttrFrStr "lang" (surnameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (surnameRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (surnameRole v)
	]
instance XmlContent Surname_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Surname_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Surname_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Surname_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Surname_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Surname_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Surname_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Surname_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Surname_Str a) = toText a
    toElem (Surname_Link a) = toElem a
    toElem (Surname_Ulink a) = toElem a
    toElem (Surname_Emphasis a) = toElem a
    toElem (Surname_Trademark a) = toElem a
    toElem (Surname_Replaceable a) = toElem a
    toElem (Surname_Inlinemediaobject a) = toElem a
instance XmlAttrType Surname_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Surname_Revisionflag_Changed
	    translate "added" = Just Surname_Revisionflag_Added
	    translate "deleted" = Just Surname_Revisionflag_Deleted
	    translate "off" = Just Surname_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Surname_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Surname_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Surname_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Surname_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Pubdate where
    fromElem (CElem (Elem "pubdate" as c0):rest) =
	(\(a,ca)->
	   (Just (Pubdate (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Pubdate as a) =
	[CElem (Elem "pubdate" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Pubdate_Attrs where
    fromAttrs as =
	Pubdate_Attrs
	  { pubdateId = possibleA fromAttrToStr "id" as
	  , pubdateLang = possibleA fromAttrToStr "lang" as
	  , pubdateRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , pubdateRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (pubdateId v)
	, maybeToAttr toAttrFrStr "lang" (pubdateLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (pubdateRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (pubdateRole v)
	]
instance XmlContent Pubdate_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Pubdate_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Pubdate_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Pubdate_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Pubdate_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Pubdate_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Pubdate_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Pubdate_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Pubdate_Str a) = toText a
    toElem (Pubdate_Link a) = toElem a
    toElem (Pubdate_Ulink a) = toElem a
    toElem (Pubdate_Emphasis a) = toElem a
    toElem (Pubdate_Trademark a) = toElem a
    toElem (Pubdate_Replaceable a) = toElem a
    toElem (Pubdate_Inlinemediaobject a) = toElem a
instance XmlAttrType Pubdate_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Pubdate_Revisionflag_Changed
	    translate "added" = Just Pubdate_Revisionflag_Added
	    translate "deleted" = Just Pubdate_Revisionflag_Deleted
	    translate "off" = Just Pubdate_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Pubdate_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Pubdate_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Pubdate_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Pubdate_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Publishername where
    fromElem (CElem (Elem "publishername" as c0):rest) =
	(\(a,ca)->
	   (Just (Publishername (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Publishername as a) =
	[CElem (Elem "publishername" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Publishername_Attrs where
    fromAttrs as =
	Publishername_Attrs
	  { publishernameId = possibleA fromAttrToStr "id" as
	  , publishernameLang = possibleA fromAttrToStr "lang" as
	  , publishernameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , publishernameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (publishernameId v)
	, maybeToAttr toAttrFrStr "lang" (publishernameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (publishernameRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (publishernameRole v)
	]
instance XmlContent Publishername_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Publishername_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Publishername_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Publishername_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Publishername_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Publishername_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Publishername_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Publishername_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Publishername_Str a) = toText a
    toElem (Publishername_Link a) = toElem a
    toElem (Publishername_Ulink a) = toElem a
    toElem (Publishername_Emphasis a) = toElem a
    toElem (Publishername_Trademark a) = toElem a
    toElem (Publishername_Replaceable a) = toElem a
    toElem (Publishername_Inlinemediaobject a) = toElem a
instance XmlAttrType Publishername_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Publishername_Revisionflag_Changed
	    translate "added" = Just Publishername_Revisionflag_Added
	    translate "deleted" = Just Publishername_Revisionflag_Deleted
	    translate "off" = Just Publishername_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Publishername_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Publishername_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Publishername_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Publishername_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Releaseinfo where
    fromElem (CElem (Elem "releaseinfo" as c0):rest) =
	(\(a,ca)->
	   (Just (Releaseinfo (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Releaseinfo as a) =
	[CElem (Elem "releaseinfo" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Releaseinfo_Attrs where
    fromAttrs as =
	Releaseinfo_Attrs
	  { releaseinfoId = possibleA fromAttrToStr "id" as
	  , releaseinfoLang = possibleA fromAttrToStr "lang" as
	  , releaseinfoRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , releaseinfoRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (releaseinfoId v)
	, maybeToAttr toAttrFrStr "lang" (releaseinfoLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (releaseinfoRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (releaseinfoRole v)
	]
instance XmlContent Releaseinfo_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Releaseinfo_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Releaseinfo_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Releaseinfo_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Releaseinfo_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Releaseinfo_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Releaseinfo_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Releaseinfo_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Releaseinfo_Str a) = toText a
    toElem (Releaseinfo_Link a) = toElem a
    toElem (Releaseinfo_Ulink a) = toElem a
    toElem (Releaseinfo_Emphasis a) = toElem a
    toElem (Releaseinfo_Trademark a) = toElem a
    toElem (Releaseinfo_Replaceable a) = toElem a
    toElem (Releaseinfo_Inlinemediaobject a) = toElem a
instance XmlAttrType Releaseinfo_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Releaseinfo_Revisionflag_Changed
	    translate "added" = Just Releaseinfo_Revisionflag_Added
	    translate "deleted" = Just Releaseinfo_Revisionflag_Deleted
	    translate "off" = Just Releaseinfo_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Releaseinfo_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Releaseinfo_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Releaseinfo_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Releaseinfo_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Revhistory where
    fromElem (CElem (Elem "revhistory" as c0):rest) =
	(\(a,ca)->
	   (Just (Revhistory (fromAttrs as) a), rest))
	(definite fromElem "revision+" "revhistory" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revhistory as a) =
	[CElem (Elem "revhistory" (toAttrs as) (toElem a))]
instance XmlAttributes Revhistory_Attrs where
    fromAttrs as =
	Revhistory_Attrs
	  { revhistoryId = possibleA fromAttrToStr "id" as
	  , revhistoryLang = possibleA fromAttrToStr "lang" as
	  , revhistoryRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , revhistoryRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (revhistoryId v)
	, maybeToAttr toAttrFrStr "lang" (revhistoryLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (revhistoryRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (revhistoryRole v)
	]
instance XmlAttrType Revhistory_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Revhistory_Revisionflag_Changed
	    translate "added" = Just Revhistory_Revisionflag_Added
	    translate "deleted" = Just Revhistory_Revisionflag_Deleted
	    translate "off" = Just Revhistory_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Revhistory_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Revhistory_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Revhistory_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Revhistory_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Revision where
    fromElem (CElem (Elem "revision" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (Just (Revision (fromAttrs as) a b c d), rest))
		 (fromElem cc))
	      (many fromElem cb))
	   (definite fromElem "<date>" "revision" ca))
	(definite fromElem "<revnumber>" "revision" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revision as a b c d) =
	[CElem (Elem "revision" (toAttrs as) (toElem a ++
					      toElem b ++ concatMap toElem c ++
					      maybe [] toElem d))]
instance XmlAttributes Revision_Attrs where
    fromAttrs as =
	Revision_Attrs
	  { revisionId = possibleA fromAttrToStr "id" as
	  , revisionLang = possibleA fromAttrToStr "lang" as
	  , revisionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , revisionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (revisionId v)
	, maybeToAttr toAttrFrStr "lang" (revisionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (revisionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (revisionRole v)
	]
instance XmlAttrType Revision_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Revision_Revisionflag_Changed
	    translate "added" = Just Revision_Revisionflag_Added
	    translate "deleted" = Just Revision_Revisionflag_Deleted
	    translate "off" = Just Revision_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Revision_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Revision_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Revision_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Revision_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Revnumber where
    fromElem (CElem (Elem "revnumber" as c0):rest) =
	(\(a,ca)->
	   (Just (Revnumber (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revnumber as a) =
	[CElem (Elem "revnumber" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Revnumber_Attrs where
    fromAttrs as =
	Revnumber_Attrs
	  { revnumberId = possibleA fromAttrToStr "id" as
	  , revnumberLang = possibleA fromAttrToStr "lang" as
	  , revnumberRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , revnumberRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (revnumberId v)
	, maybeToAttr toAttrFrStr "lang" (revnumberLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (revnumberRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (revnumberRole v)
	]
instance XmlContent Revnumber_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Revnumber_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Revnumber_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Revnumber_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Revnumber_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Revnumber_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Revnumber_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Revnumber_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revnumber_Str a) = toText a
    toElem (Revnumber_Link a) = toElem a
    toElem (Revnumber_Ulink a) = toElem a
    toElem (Revnumber_Emphasis a) = toElem a
    toElem (Revnumber_Trademark a) = toElem a
    toElem (Revnumber_Replaceable a) = toElem a
    toElem (Revnumber_Inlinemediaobject a) = toElem a
instance XmlAttrType Revnumber_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Revnumber_Revisionflag_Changed
	    translate "added" = Just Revnumber_Revisionflag_Added
	    translate "deleted" = Just Revnumber_Revisionflag_Deleted
	    translate "off" = Just Revnumber_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Revnumber_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Revnumber_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Revnumber_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Revnumber_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Revremark where
    fromElem (CElem (Elem "revremark" as c0):rest) =
	(\(a,ca)->
	   (Just (Revremark (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revremark as a) =
	[CElem (Elem "revremark" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Revremark_Attrs where
    fromAttrs as =
	Revremark_Attrs
	  { revremarkId = possibleA fromAttrToStr "id" as
	  , revremarkLang = possibleA fromAttrToStr "lang" as
	  , revremarkRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , revremarkRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (revremarkId v)
	, maybeToAttr toAttrFrStr "lang" (revremarkLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (revremarkRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (revremarkRole v)
	]
instance XmlContent Revremark_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Revremark_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Revremark_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Revremark_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Revremark_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Revremark_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Revremark_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Revremark_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revremark_Str a) = toText a
    toElem (Revremark_Link a) = toElem a
    toElem (Revremark_Ulink a) = toElem a
    toElem (Revremark_Emphasis a) = toElem a
    toElem (Revremark_Trademark a) = toElem a
    toElem (Revremark_Replaceable a) = toElem a
    toElem (Revremark_Inlinemediaobject a) = toElem a
instance XmlAttrType Revremark_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Revremark_Revisionflag_Changed
	    translate "added" = Just Revremark_Revisionflag_Added
	    translate "deleted" = Just Revremark_Revisionflag_Deleted
	    translate "off" = Just Revremark_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Revremark_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Revremark_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Revremark_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Revremark_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Revdescription where
    fromElem (CElem (Elem "revdescription" as c0):rest) =
	(\(a,ca)->
	   (Just (Revdescription (fromAttrs as) a), rest))
	(definite fromElem "(itemizedlist|orderedlist|variablelist|note|literallayout|programlisting|para|blockquote|mediaobject|informaltable|example|figure|table)+" "revdescription" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Revdescription as a) =
	[CElem (Elem "revdescription" (toAttrs as) (toElem a))]
instance XmlAttributes Revdescription_Attrs where
    fromAttrs as =
	Revdescription_Attrs
	  { revdescriptionId = possibleA fromAttrToStr "id" as
	  , revdescriptionLang = possibleA fromAttrToStr "lang" as
	  , revdescriptionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , revdescriptionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (revdescriptionId v)
	, maybeToAttr toAttrFrStr "lang" (revdescriptionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (revdescriptionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (revdescriptionRole v)
	]
instance XmlAttrType Revdescription_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Revdescription_Revisionflag_Changed
	    translate "added" = Just Revdescription_Revisionflag_Added
	    translate "deleted" = Just Revdescription_Revisionflag_Deleted
	    translate "off" = Just Revdescription_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Revdescription_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Revdescription_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Revdescription_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Revdescription_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Volumenum where
    fromElem (CElem (Elem "volumenum" as c0):rest) =
	(\(a,ca)->
	   (Just (Volumenum (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Volumenum as a) =
	[CElem (Elem "volumenum" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Volumenum_Attrs where
    fromAttrs as =
	Volumenum_Attrs
	  { volumenumId = possibleA fromAttrToStr "id" as
	  , volumenumLang = possibleA fromAttrToStr "lang" as
	  , volumenumRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , volumenumRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (volumenumId v)
	, maybeToAttr toAttrFrStr "lang" (volumenumLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (volumenumRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (volumenumRole v)
	]
instance XmlContent Volumenum_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Volumenum_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Volumenum_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Volumenum_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Volumenum_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Volumenum_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Volumenum_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Volumenum_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Volumenum_Str a) = toText a
    toElem (Volumenum_Link a) = toElem a
    toElem (Volumenum_Ulink a) = toElem a
    toElem (Volumenum_Emphasis a) = toElem a
    toElem (Volumenum_Trademark a) = toElem a
    toElem (Volumenum_Replaceable a) = toElem a
    toElem (Volumenum_Inlinemediaobject a) = toElem a
instance XmlAttrType Volumenum_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Volumenum_Revisionflag_Changed
	    translate "added" = Just Volumenum_Revisionflag_Added
	    translate "deleted" = Just Volumenum_Revisionflag_Deleted
	    translate "off" = Just Volumenum_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Volumenum_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Volumenum_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Volumenum_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Volumenum_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Command where
    fromElem (CElem (Elem "command" as c0):rest) =
	(\(a,ca)->
	   (Just (Command (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Command as a) =
	[CElem (Elem "command" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Command_Attrs where
    fromAttrs as =
	Command_Attrs
	  { commandMoreinfo = defaultA fromAttrToTyp Command_Moreinfo_None "moreinfo" as
	  , commandId = possibleA fromAttrToStr "id" as
	  , commandLang = possibleA fromAttrToStr "lang" as
	  , commandRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , commandRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "moreinfo" (commandMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (commandId v)
	, maybeToAttr toAttrFrStr "lang" (commandLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (commandRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (commandRole v)
	]
instance XmlContent Command_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Command_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Command_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Command_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Command_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Command_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Command_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Command_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Command_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Command_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Command_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Command_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Command_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Command_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Command_Str a) = toText a
    toElem (Command_Link a) = toElem a
    toElem (Command_Ulink a) = toElem a
    toElem (Command_Command a) = toElem a
    toElem (Command_Computeroutput a) = toElem a
    toElem (Command_Email a) = toElem a
    toElem (Command_Filename a) = toElem a
    toElem (Command_Literal a) = toElem a
    toElem (Command_Option a) = toElem a
    toElem (Command_Replaceable a) = toElem a
    toElem (Command_Systemitem a) = toElem a
    toElem (Command_Userinput a) = toElem a
    toElem (Command_Inlinemediaobject a) = toElem a
instance XmlAttrType Command_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Command_Moreinfo_Refentry
	    translate "none" = Just Command_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Command_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Command_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Command_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Command_Revisionflag_Changed
	    translate "added" = Just Command_Revisionflag_Added
	    translate "deleted" = Just Command_Revisionflag_Deleted
	    translate "off" = Just Command_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Command_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Command_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Command_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Command_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Computeroutput where
    fromElem (CElem (Elem "computeroutput" as c0):rest) =
	(\(a,ca)->
	   (Just (Computeroutput (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Computeroutput as a) =
	[CElem (Elem "computeroutput" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Computeroutput_Attrs where
    fromAttrs as =
	Computeroutput_Attrs
	  { computeroutputMoreinfo = defaultA fromAttrToTyp Computeroutput_Moreinfo_None "moreinfo" as
	  , computeroutputId = possibleA fromAttrToStr "id" as
	  , computeroutputLang = possibleA fromAttrToStr "lang" as
	  , computeroutputRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , computeroutputRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "moreinfo" (computeroutputMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (computeroutputId v)
	, maybeToAttr toAttrFrStr "lang" (computeroutputLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (computeroutputRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (computeroutputRole v)
	]
instance XmlContent Computeroutput_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Computeroutput_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Computeroutput_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Computeroutput_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Computeroutput_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Computeroutput_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Computeroutput_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Computeroutput_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Computeroutput_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Computeroutput_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Computeroutput_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Computeroutput_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Computeroutput_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Computeroutput_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Computeroutput_Str a) = toText a
    toElem (Computeroutput_Link a) = toElem a
    toElem (Computeroutput_Ulink a) = toElem a
    toElem (Computeroutput_Command a) = toElem a
    toElem (Computeroutput_Computeroutput a) = toElem a
    toElem (Computeroutput_Email a) = toElem a
    toElem (Computeroutput_Filename a) = toElem a
    toElem (Computeroutput_Literal a) = toElem a
    toElem (Computeroutput_Option a) = toElem a
    toElem (Computeroutput_Replaceable a) = toElem a
    toElem (Computeroutput_Systemitem a) = toElem a
    toElem (Computeroutput_Userinput a) = toElem a
    toElem (Computeroutput_Inlinemediaobject a) = toElem a
instance XmlAttrType Computeroutput_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Computeroutput_Moreinfo_Refentry
	    translate "none" = Just Computeroutput_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Computeroutput_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Computeroutput_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Computeroutput_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Computeroutput_Revisionflag_Changed
	    translate "added" = Just Computeroutput_Revisionflag_Added
	    translate "deleted" = Just Computeroutput_Revisionflag_Deleted
	    translate "off" = Just Computeroutput_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Computeroutput_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Computeroutput_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Computeroutput_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Computeroutput_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Email where
    fromElem (CElem (Elem "email" as c0):rest) =
	(\(a,ca)->
	   (Just (Email (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Email as a) =
	[CElem (Elem "email" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Email_Attrs where
    fromAttrs as =
	Email_Attrs
	  { emailId = possibleA fromAttrToStr "id" as
	  , emailLang = possibleA fromAttrToStr "lang" as
	  , emailRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , emailRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (emailId v)
	, maybeToAttr toAttrFrStr "lang" (emailLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (emailRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (emailRole v)
	]
instance XmlContent Email_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Email_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Email_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Email_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Email_Emphasis a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Email_Trademark a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Email_Replaceable a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Email_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Email_Str a) = toText a
    toElem (Email_Link a) = toElem a
    toElem (Email_Ulink a) = toElem a
    toElem (Email_Emphasis a) = toElem a
    toElem (Email_Trademark a) = toElem a
    toElem (Email_Replaceable a) = toElem a
    toElem (Email_Inlinemediaobject a) = toElem a
instance XmlAttrType Email_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Email_Revisionflag_Changed
	    translate "added" = Just Email_Revisionflag_Added
	    translate "deleted" = Just Email_Revisionflag_Deleted
	    translate "off" = Just Email_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Email_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Email_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Email_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Email_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Filename where
    fromElem (CElem (Elem "filename" as c0):rest) =
	(\(a,ca)->
	   (Just (Filename (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Filename as a) =
	[CElem (Elem "filename" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Filename_Attrs where
    fromAttrs as =
	Filename_Attrs
	  { filenameClass = possibleA fromAttrToTyp "class" as
	  , filenamePath = possibleA fromAttrToStr "path" as
	  , filenameMoreinfo = defaultA fromAttrToTyp Filename_Moreinfo_None "moreinfo" as
	  , filenameId = possibleA fromAttrToStr "id" as
	  , filenameLang = possibleA fromAttrToStr "lang" as
	  , filenameRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , filenameRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "class" (filenameClass v)
	, maybeToAttr toAttrFrStr "path" (filenamePath v)
	, defaultToAttr toAttrFrTyp "moreinfo" (filenameMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (filenameId v)
	, maybeToAttr toAttrFrStr "lang" (filenameLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (filenameRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (filenameRole v)
	]
instance XmlContent Filename_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Filename_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Filename_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Filename_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Filename_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Filename_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Filename_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Filename_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Filename_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Filename_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Filename_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Filename_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Filename_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Filename_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Filename_Str a) = toText a
    toElem (Filename_Link a) = toElem a
    toElem (Filename_Ulink a) = toElem a
    toElem (Filename_Command a) = toElem a
    toElem (Filename_Computeroutput a) = toElem a
    toElem (Filename_Email a) = toElem a
    toElem (Filename_Filename a) = toElem a
    toElem (Filename_Literal a) = toElem a
    toElem (Filename_Option a) = toElem a
    toElem (Filename_Replaceable a) = toElem a
    toElem (Filename_Systemitem a) = toElem a
    toElem (Filename_Userinput a) = toElem a
    toElem (Filename_Inlinemediaobject a) = toElem a
instance XmlAttrType Filename_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "headerfile" = Just Filename_Class_Headerfile
	    translate "partition" = Just Filename_Class_Partition
	    translate "devicefile" = Just Filename_Class_Devicefile
	    translate "libraryfile" = Just Filename_Class_Libraryfile
	    translate "directory" = Just Filename_Class_Directory
	    translate "extension" = Just Filename_Class_Extension
	    translate "symlink" = Just Filename_Class_Symlink
	    translate _ = Nothing
    toAttrFrTyp n Filename_Class_Headerfile = Just (n, str2attr "headerfile")
    toAttrFrTyp n Filename_Class_Partition = Just (n, str2attr "partition")
    toAttrFrTyp n Filename_Class_Devicefile = Just (n, str2attr "devicefile")
    toAttrFrTyp n Filename_Class_Libraryfile = Just (n, str2attr "libraryfile")
    toAttrFrTyp n Filename_Class_Directory = Just (n, str2attr "directory")
    toAttrFrTyp n Filename_Class_Extension = Just (n, str2attr "extension")
    toAttrFrTyp n Filename_Class_Symlink = Just (n, str2attr "symlink")
instance XmlAttrType Filename_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Filename_Moreinfo_Refentry
	    translate "none" = Just Filename_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Filename_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Filename_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Filename_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Filename_Revisionflag_Changed
	    translate "added" = Just Filename_Revisionflag_Added
	    translate "deleted" = Just Filename_Revisionflag_Deleted
	    translate "off" = Just Filename_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Filename_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Filename_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Filename_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Filename_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Lineannotation where
    fromElem (CElem (Elem "lineannotation" as c0):rest) =
	(\(a,ca)->
	   (Just (Lineannotation (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Lineannotation as a) =
	[CElem (Elem "lineannotation" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Lineannotation_Attrs where
    fromAttrs as =
	Lineannotation_Attrs
	  { lineannotationId = possibleA fromAttrToStr "id" as
	  , lineannotationLang = possibleA fromAttrToStr "lang" as
	  , lineannotationRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , lineannotationRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (lineannotationId v)
	, maybeToAttr toAttrFrStr "lang" (lineannotationLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (lineannotationRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (lineannotationRole v)
	]
instance XmlContent Lineannotation_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Lineannotation_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Lineannotation_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Lineannotation_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Lineannotation_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Lineannotation_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Lineannotation_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Lineannotation_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Lineannotation_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Lineannotation_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Lineannotation_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Lineannotation_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Lineannotation_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Lineannotation_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Lineannotation_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Lineannotation_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Lineannotation_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Lineannotation_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Lineannotation_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Lineannotation_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Lineannotation_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Lineannotation_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Lineannotation_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Lineannotation_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Lineannotation_Str a) = toText a
    toElem (Lineannotation_Footnoteref a) = toElem a
    toElem (Lineannotation_Xref a) = toElem a
    toElem (Lineannotation_Abbrev a) = toElem a
    toElem (Lineannotation_Acronym a) = toElem a
    toElem (Lineannotation_Citetitle a) = toElem a
    toElem (Lineannotation_Emphasis a) = toElem a
    toElem (Lineannotation_Footnote a) = toElem a
    toElem (Lineannotation_Phrase a) = toElem a
    toElem (Lineannotation_Quote a) = toElem a
    toElem (Lineannotation_Trademark a) = toElem a
    toElem (Lineannotation_Link a) = toElem a
    toElem (Lineannotation_Ulink a) = toElem a
    toElem (Lineannotation_Command a) = toElem a
    toElem (Lineannotation_Computeroutput a) = toElem a
    toElem (Lineannotation_Email a) = toElem a
    toElem (Lineannotation_Filename a) = toElem a
    toElem (Lineannotation_Literal a) = toElem a
    toElem (Lineannotation_Option a) = toElem a
    toElem (Lineannotation_Replaceable a) = toElem a
    toElem (Lineannotation_Systemitem a) = toElem a
    toElem (Lineannotation_Userinput a) = toElem a
    toElem (Lineannotation_Inlinemediaobject a) = toElem a
instance XmlAttrType Lineannotation_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Lineannotation_Revisionflag_Changed
	    translate "added" = Just Lineannotation_Revisionflag_Added
	    translate "deleted" = Just Lineannotation_Revisionflag_Deleted
	    translate "off" = Just Lineannotation_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Lineannotation_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Lineannotation_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Lineannotation_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Lineannotation_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Literal where
    fromElem (CElem (Elem "literal" as c0):rest) =
	(\(a,ca)->
	   (Just (Literal (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Literal as a) =
	[CElem (Elem "literal" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Literal_Attrs where
    fromAttrs as =
	Literal_Attrs
	  { literalMoreinfo = defaultA fromAttrToTyp Literal_Moreinfo_None "moreinfo" as
	  , literalId = possibleA fromAttrToStr "id" as
	  , literalLang = possibleA fromAttrToStr "lang" as
	  , literalRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , literalRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "moreinfo" (literalMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (literalId v)
	, maybeToAttr toAttrFrStr "lang" (literalLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (literalRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (literalRole v)
	]
instance XmlContent Literal_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Literal_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Literal_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Literal_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Literal_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Literal_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Literal_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Literal_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Literal_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Literal_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Literal_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Literal_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Literal_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Literal_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Literal_Str a) = toText a
    toElem (Literal_Link a) = toElem a
    toElem (Literal_Ulink a) = toElem a
    toElem (Literal_Command a) = toElem a
    toElem (Literal_Computeroutput a) = toElem a
    toElem (Literal_Email a) = toElem a
    toElem (Literal_Filename a) = toElem a
    toElem (Literal_Literal a) = toElem a
    toElem (Literal_Option a) = toElem a
    toElem (Literal_Replaceable a) = toElem a
    toElem (Literal_Systemitem a) = toElem a
    toElem (Literal_Userinput a) = toElem a
    toElem (Literal_Inlinemediaobject a) = toElem a
instance XmlAttrType Literal_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Literal_Moreinfo_Refentry
	    translate "none" = Just Literal_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Literal_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Literal_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Literal_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Literal_Revisionflag_Changed
	    translate "added" = Just Literal_Revisionflag_Added
	    translate "deleted" = Just Literal_Revisionflag_Deleted
	    translate "off" = Just Literal_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Literal_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Literal_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Literal_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Literal_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Option where
    fromElem (CElem (Elem "option" as c0):rest) =
	(\(a,ca)->
	   (Just (Option (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Option as a) =
	[CElem (Elem "option" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Option_Attrs where
    fromAttrs as =
	Option_Attrs
	  { optionId = possibleA fromAttrToStr "id" as
	  , optionLang = possibleA fromAttrToStr "lang" as
	  , optionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , optionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (optionId v)
	, maybeToAttr toAttrFrStr "lang" (optionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (optionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (optionRole v)
	]
instance XmlContent Option_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Option_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Option_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Option_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Option_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Option_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Option_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Option_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Option_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Option_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Option_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Option_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Option_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Option_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Option_Str a) = toText a
    toElem (Option_Link a) = toElem a
    toElem (Option_Ulink a) = toElem a
    toElem (Option_Command a) = toElem a
    toElem (Option_Computeroutput a) = toElem a
    toElem (Option_Email a) = toElem a
    toElem (Option_Filename a) = toElem a
    toElem (Option_Literal a) = toElem a
    toElem (Option_Option a) = toElem a
    toElem (Option_Replaceable a) = toElem a
    toElem (Option_Systemitem a) = toElem a
    toElem (Option_Userinput a) = toElem a
    toElem (Option_Inlinemediaobject a) = toElem a
instance XmlAttrType Option_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Option_Revisionflag_Changed
	    translate "added" = Just Option_Revisionflag_Added
	    translate "deleted" = Just Option_Revisionflag_Deleted
	    translate "off" = Just Option_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Option_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Option_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Option_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Option_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Replaceable where
    fromElem (CElem (Elem "replaceable" as c0):rest) =
	(\(a,ca)->
	   (Just (Replaceable (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Replaceable as a) =
	[CElem (Elem "replaceable" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Replaceable_Attrs where
    fromAttrs as =
	Replaceable_Attrs
	  { replaceableClass = possibleA fromAttrToTyp "class" as
	  , replaceableId = possibleA fromAttrToStr "id" as
	  , replaceableLang = possibleA fromAttrToStr "lang" as
	  , replaceableRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , replaceableRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "class" (replaceableClass v)
	, maybeToAttr toAttrFrStr "id" (replaceableId v)
	, maybeToAttr toAttrFrStr "lang" (replaceableLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (replaceableRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (replaceableRole v)
	]
instance XmlContent Replaceable_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Replaceable_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Replaceable_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Replaceable_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Replaceable_Inlinemediaobject a), rest)
				(_,_) ->
				    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Replaceable_Str a) = toText a
    toElem (Replaceable_Link a) = toElem a
    toElem (Replaceable_Ulink a) = toElem a
    toElem (Replaceable_Inlinemediaobject a) = toElem a
instance XmlAttrType Replaceable_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "command" = Just Replaceable_Class_Command
	    translate "function" = Just Replaceable_Class_Function
	    translate "option" = Just Replaceable_Class_Option
	    translate "parameter" = Just Replaceable_Class_Parameter
	    translate _ = Nothing
    toAttrFrTyp n Replaceable_Class_Command = Just (n, str2attr "command")
    toAttrFrTyp n Replaceable_Class_Function = Just (n, str2attr "function")
    toAttrFrTyp n Replaceable_Class_Option = Just (n, str2attr "option")
    toAttrFrTyp n Replaceable_Class_Parameter = Just (n, str2attr "parameter")
instance XmlAttrType Replaceable_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Replaceable_Revisionflag_Changed
	    translate "added" = Just Replaceable_Revisionflag_Added
	    translate "deleted" = Just Replaceable_Revisionflag_Deleted
	    translate "off" = Just Replaceable_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Replaceable_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Replaceable_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Replaceable_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Replaceable_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Systemitem where
    fromElem (CElem (Elem "systemitem" as c0):rest) =
	(\(a,ca)->
	   (Just (Systemitem (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Systemitem as a) =
	[CElem (Elem "systemitem" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Systemitem_Attrs where
    fromAttrs as =
	Systemitem_Attrs
	  { systemitemClass = possibleA fromAttrToTyp "class" as
	  , systemitemMoreinfo = defaultA fromAttrToTyp Systemitem_Moreinfo_None "moreinfo" as
	  , systemitemId = possibleA fromAttrToStr "id" as
	  , systemitemLang = possibleA fromAttrToStr "lang" as
	  , systemitemRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , systemitemRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "class" (systemitemClass v)
	, defaultToAttr toAttrFrTyp "moreinfo" (systemitemMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (systemitemId v)
	, maybeToAttr toAttrFrStr "lang" (systemitemLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (systemitemRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (systemitemRole v)
	]
instance XmlContent Systemitem_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Systemitem_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Systemitem_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Systemitem_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Systemitem_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Systemitem_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Systemitem_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Systemitem_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Systemitem_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Systemitem_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Systemitem_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Systemitem_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Systemitem_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Systemitem_Inlinemediaobject a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Systemitem_Acronym a), rest)
														(_,_) ->
														    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Systemitem_Str a) = toText a
    toElem (Systemitem_Link a) = toElem a
    toElem (Systemitem_Ulink a) = toElem a
    toElem (Systemitem_Command a) = toElem a
    toElem (Systemitem_Computeroutput a) = toElem a
    toElem (Systemitem_Email a) = toElem a
    toElem (Systemitem_Filename a) = toElem a
    toElem (Systemitem_Literal a) = toElem a
    toElem (Systemitem_Option a) = toElem a
    toElem (Systemitem_Replaceable a) = toElem a
    toElem (Systemitem_Systemitem a) = toElem a
    toElem (Systemitem_Userinput a) = toElem a
    toElem (Systemitem_Inlinemediaobject a) = toElem a
    toElem (Systemitem_Acronym a) = toElem a
instance XmlAttrType Systemitem_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "constant" = Just Systemitem_Class_Constant
	    translate "event" = Just Systemitem_Class_Event
	    translate "eventhandler" = Just Systemitem_Class_Eventhandler
	    translate "domainname" = Just Systemitem_Class_Domainname
	    translate "fqdomainname" = Just Systemitem_Class_Fqdomainname
	    translate "ipaddress" = Just Systemitem_Class_Ipaddress
	    translate "netmask" = Just Systemitem_Class_Netmask
	    translate "etheraddress" = Just Systemitem_Class_Etheraddress
	    translate "groupname" = Just Systemitem_Class_Groupname
	    translate "library" = Just Systemitem_Class_Library
	    translate "macro" = Just Systemitem_Class_Macro
	    translate "osname" = Just Systemitem_Class_Osname
	    translate "filesystem" = Just Systemitem_Class_Filesystem
	    translate "resource" = Just Systemitem_Class_Resource
	    translate "systemname" = Just Systemitem_Class_Systemname
	    translate "username" = Just Systemitem_Class_Username
	    translate "newsgroup" = Just Systemitem_Class_Newsgroup
	    translate _ = Nothing
    toAttrFrTyp n Systemitem_Class_Constant = Just (n, str2attr "constant")
    toAttrFrTyp n Systemitem_Class_Event = Just (n, str2attr "event")
    toAttrFrTyp n Systemitem_Class_Eventhandler = Just (n, str2attr "eventhandler")
    toAttrFrTyp n Systemitem_Class_Domainname = Just (n, str2attr "domainname")
    toAttrFrTyp n Systemitem_Class_Fqdomainname = Just (n, str2attr "fqdomainname")
    toAttrFrTyp n Systemitem_Class_Ipaddress = Just (n, str2attr "ipaddress")
    toAttrFrTyp n Systemitem_Class_Netmask = Just (n, str2attr "netmask")
    toAttrFrTyp n Systemitem_Class_Etheraddress = Just (n, str2attr "etheraddress")
    toAttrFrTyp n Systemitem_Class_Groupname = Just (n, str2attr "groupname")
    toAttrFrTyp n Systemitem_Class_Library = Just (n, str2attr "library")
    toAttrFrTyp n Systemitem_Class_Macro = Just (n, str2attr "macro")
    toAttrFrTyp n Systemitem_Class_Osname = Just (n, str2attr "osname")
    toAttrFrTyp n Systemitem_Class_Filesystem = Just (n, str2attr "filesystem")
    toAttrFrTyp n Systemitem_Class_Resource = Just (n, str2attr "resource")
    toAttrFrTyp n Systemitem_Class_Systemname = Just (n, str2attr "systemname")
    toAttrFrTyp n Systemitem_Class_Username = Just (n, str2attr "username")
    toAttrFrTyp n Systemitem_Class_Newsgroup = Just (n, str2attr "newsgroup")
instance XmlAttrType Systemitem_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Systemitem_Moreinfo_Refentry
	    translate "none" = Just Systemitem_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Systemitem_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Systemitem_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Systemitem_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Systemitem_Revisionflag_Changed
	    translate "added" = Just Systemitem_Revisionflag_Added
	    translate "deleted" = Just Systemitem_Revisionflag_Deleted
	    translate "off" = Just Systemitem_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Systemitem_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Systemitem_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Systemitem_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Systemitem_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Userinput where
    fromElem (CElem (Elem "userinput" as c0):rest) =
	(\(a,ca)->
	   (Just (Userinput (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Userinput as a) =
	[CElem (Elem "userinput" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Userinput_Attrs where
    fromAttrs as =
	Userinput_Attrs
	  { userinputMoreinfo = defaultA fromAttrToTyp Userinput_Moreinfo_None "moreinfo" as
	  , userinputId = possibleA fromAttrToStr "id" as
	  , userinputLang = possibleA fromAttrToStr "lang" as
	  , userinputRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , userinputRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "moreinfo" (userinputMoreinfo v)
	, maybeToAttr toAttrFrStr "id" (userinputId v)
	, maybeToAttr toAttrFrStr "lang" (userinputLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (userinputRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (userinputRole v)
	]
instance XmlContent Userinput_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Userinput_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Userinput_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Userinput_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Userinput_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Userinput_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Userinput_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Userinput_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Userinput_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Userinput_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Userinput_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Userinput_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Userinput_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Userinput_Inlinemediaobject a), rest)
													(_,_) ->
													    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Userinput_Str a) = toText a
    toElem (Userinput_Link a) = toElem a
    toElem (Userinput_Ulink a) = toElem a
    toElem (Userinput_Command a) = toElem a
    toElem (Userinput_Computeroutput a) = toElem a
    toElem (Userinput_Email a) = toElem a
    toElem (Userinput_Filename a) = toElem a
    toElem (Userinput_Literal a) = toElem a
    toElem (Userinput_Option a) = toElem a
    toElem (Userinput_Replaceable a) = toElem a
    toElem (Userinput_Systemitem a) = toElem a
    toElem (Userinput_Userinput a) = toElem a
    toElem (Userinput_Inlinemediaobject a) = toElem a
instance XmlAttrType Userinput_Moreinfo where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "refentry" = Just Userinput_Moreinfo_Refentry
	    translate "none" = Just Userinput_Moreinfo_None
	    translate _ = Nothing
    toAttrFrTyp n Userinput_Moreinfo_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Userinput_Moreinfo_None = Just (n, str2attr "none")
instance XmlAttrType Userinput_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Userinput_Revisionflag_Changed
	    translate "added" = Just Userinput_Revisionflag_Added
	    translate "deleted" = Just Userinput_Revisionflag_Deleted
	    translate "off" = Just Userinput_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Userinput_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Userinput_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Userinput_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Userinput_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Abbrev where
    fromElem (CElem (Elem "abbrev" as c0):rest) =
	(\(a,ca)->
	   (Just (Abbrev (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Abbrev as a) =
	[CElem (Elem "abbrev" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Abbrev_Attrs where
    fromAttrs as =
	Abbrev_Attrs
	  { abbrevId = possibleA fromAttrToStr "id" as
	  , abbrevLang = possibleA fromAttrToStr "lang" as
	  , abbrevRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , abbrevRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (abbrevId v)
	, maybeToAttr toAttrFrStr "lang" (abbrevLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (abbrevRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (abbrevRole v)
	]
instance XmlContent Abbrev_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Abbrev_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Abbrev_Acronym a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Abbrev_Emphasis a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Abbrev_Trademark a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Abbrev_Link a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Abbrev_Ulink a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Abbrev_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Abbrev_Str a) = toText a
    toElem (Abbrev_Acronym a) = toElem a
    toElem (Abbrev_Emphasis a) = toElem a
    toElem (Abbrev_Trademark a) = toElem a
    toElem (Abbrev_Link a) = toElem a
    toElem (Abbrev_Ulink a) = toElem a
    toElem (Abbrev_Inlinemediaobject a) = toElem a
instance XmlAttrType Abbrev_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Abbrev_Revisionflag_Changed
	    translate "added" = Just Abbrev_Revisionflag_Added
	    translate "deleted" = Just Abbrev_Revisionflag_Deleted
	    translate "off" = Just Abbrev_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Abbrev_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Abbrev_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Abbrev_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Abbrev_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Acronym where
    fromElem (CElem (Elem "acronym" as c0):rest) =
	(\(a,ca)->
	   (Just (Acronym (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Acronym as a) =
	[CElem (Elem "acronym" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Acronym_Attrs where
    fromAttrs as =
	Acronym_Attrs
	  { acronymId = possibleA fromAttrToStr "id" as
	  , acronymLang = possibleA fromAttrToStr "lang" as
	  , acronymRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , acronymRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (acronymId v)
	, maybeToAttr toAttrFrStr "lang" (acronymLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (acronymRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (acronymRole v)
	]
instance XmlContent Acronym_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Acronym_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Acronym_Acronym a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Acronym_Emphasis a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Acronym_Trademark a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Acronym_Link a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Acronym_Ulink a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Acronym_Inlinemediaobject a), rest)
							(_,_) ->
							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Acronym_Str a) = toText a
    toElem (Acronym_Acronym a) = toElem a
    toElem (Acronym_Emphasis a) = toElem a
    toElem (Acronym_Trademark a) = toElem a
    toElem (Acronym_Link a) = toElem a
    toElem (Acronym_Ulink a) = toElem a
    toElem (Acronym_Inlinemediaobject a) = toElem a
instance XmlAttrType Acronym_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Acronym_Revisionflag_Changed
	    translate "added" = Just Acronym_Revisionflag_Added
	    translate "deleted" = Just Acronym_Revisionflag_Deleted
	    translate "off" = Just Acronym_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Acronym_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Acronym_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Acronym_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Acronym_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Citetitle where
    fromElem (CElem (Elem "citetitle" as c0):rest) =
	(\(a,ca)->
	   (Just (Citetitle (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Citetitle as a) =
	[CElem (Elem "citetitle" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Citetitle_Attrs where
    fromAttrs as =
	Citetitle_Attrs
	  { citetitlePubwork = possibleA fromAttrToTyp "pubwork" as
	  , citetitleId = possibleA fromAttrToStr "id" as
	  , citetitleLang = possibleA fromAttrToStr "lang" as
	  , citetitleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , citetitleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "pubwork" (citetitlePubwork v)
	, maybeToAttr toAttrFrStr "id" (citetitleId v)
	, maybeToAttr toAttrFrStr "lang" (citetitleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (citetitleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (citetitleRole v)
	]
instance XmlContent Citetitle_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Citetitle_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Citetitle_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Citetitle_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Citetitle_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Citetitle_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Citetitle_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Citetitle_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Citetitle_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Citetitle_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Citetitle_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Citetitle_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Citetitle_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Citetitle_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Citetitle_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Citetitle_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Citetitle_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Citetitle_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Citetitle_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Citetitle_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Citetitle_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Citetitle_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Citetitle_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Citetitle_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Citetitle_Str a) = toText a
    toElem (Citetitle_Footnoteref a) = toElem a
    toElem (Citetitle_Xref a) = toElem a
    toElem (Citetitle_Abbrev a) = toElem a
    toElem (Citetitle_Acronym a) = toElem a
    toElem (Citetitle_Citetitle a) = toElem a
    toElem (Citetitle_Emphasis a) = toElem a
    toElem (Citetitle_Footnote a) = toElem a
    toElem (Citetitle_Phrase a) = toElem a
    toElem (Citetitle_Quote a) = toElem a
    toElem (Citetitle_Trademark a) = toElem a
    toElem (Citetitle_Link a) = toElem a
    toElem (Citetitle_Ulink a) = toElem a
    toElem (Citetitle_Command a) = toElem a
    toElem (Citetitle_Computeroutput a) = toElem a
    toElem (Citetitle_Email a) = toElem a
    toElem (Citetitle_Filename a) = toElem a
    toElem (Citetitle_Literal a) = toElem a
    toElem (Citetitle_Option a) = toElem a
    toElem (Citetitle_Replaceable a) = toElem a
    toElem (Citetitle_Systemitem a) = toElem a
    toElem (Citetitle_Userinput a) = toElem a
    toElem (Citetitle_Inlinemediaobject a) = toElem a
instance XmlAttrType Citetitle_Pubwork where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "article" = Just Citetitle_Pubwork_Article
	    translate "book" = Just Citetitle_Pubwork_Book
	    translate "chapter" = Just Citetitle_Pubwork_Chapter
	    translate "part" = Just Citetitle_Pubwork_Part
	    translate "refentry" = Just Citetitle_Pubwork_Refentry
	    translate "section" = Just Citetitle_Pubwork_Section
	    translate "journal" = Just Citetitle_Pubwork_Journal
	    translate "series" = Just Citetitle_Pubwork_Series
	    translate "set" = Just Citetitle_Pubwork_Set
	    translate "manuscript" = Just Citetitle_Pubwork_Manuscript
	    translate _ = Nothing
    toAttrFrTyp n Citetitle_Pubwork_Article = Just (n, str2attr "article")
    toAttrFrTyp n Citetitle_Pubwork_Book = Just (n, str2attr "book")
    toAttrFrTyp n Citetitle_Pubwork_Chapter = Just (n, str2attr "chapter")
    toAttrFrTyp n Citetitle_Pubwork_Part = Just (n, str2attr "part")
    toAttrFrTyp n Citetitle_Pubwork_Refentry = Just (n, str2attr "refentry")
    toAttrFrTyp n Citetitle_Pubwork_Section = Just (n, str2attr "section")
    toAttrFrTyp n Citetitle_Pubwork_Journal = Just (n, str2attr "journal")
    toAttrFrTyp n Citetitle_Pubwork_Series = Just (n, str2attr "series")
    toAttrFrTyp n Citetitle_Pubwork_Set = Just (n, str2attr "set")
    toAttrFrTyp n Citetitle_Pubwork_Manuscript = Just (n, str2attr "manuscript")
instance XmlAttrType Citetitle_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Citetitle_Revisionflag_Changed
	    translate "added" = Just Citetitle_Revisionflag_Added
	    translate "deleted" = Just Citetitle_Revisionflag_Deleted
	    translate "off" = Just Citetitle_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Citetitle_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Citetitle_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Citetitle_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Citetitle_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Emphasis where
    fromElem (CElem (Elem "emphasis" as c0):rest) =
	(\(a,ca)->
	   (Just (Emphasis (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Emphasis as a) =
	[CElem (Elem "emphasis" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Emphasis_Attrs where
    fromAttrs as =
	Emphasis_Attrs
	  { emphasisId = possibleA fromAttrToStr "id" as
	  , emphasisLang = possibleA fromAttrToStr "lang" as
	  , emphasisRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , emphasisRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (emphasisId v)
	, maybeToAttr toAttrFrStr "lang" (emphasisLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (emphasisRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (emphasisRole v)
	]
instance XmlContent Emphasis_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Emphasis_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Emphasis_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Emphasis_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Emphasis_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Emphasis_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Emphasis_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Emphasis_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Emphasis_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Emphasis_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Emphasis_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Emphasis_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Emphasis_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Emphasis_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Emphasis_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Emphasis_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Emphasis_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Emphasis_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Emphasis_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Emphasis_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Emphasis_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Emphasis_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Emphasis_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Emphasis_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Emphasis_Str a) = toText a
    toElem (Emphasis_Footnoteref a) = toElem a
    toElem (Emphasis_Xref a) = toElem a
    toElem (Emphasis_Abbrev a) = toElem a
    toElem (Emphasis_Acronym a) = toElem a
    toElem (Emphasis_Citetitle a) = toElem a
    toElem (Emphasis_Emphasis a) = toElem a
    toElem (Emphasis_Footnote a) = toElem a
    toElem (Emphasis_Phrase a) = toElem a
    toElem (Emphasis_Quote a) = toElem a
    toElem (Emphasis_Trademark a) = toElem a
    toElem (Emphasis_Link a) = toElem a
    toElem (Emphasis_Ulink a) = toElem a
    toElem (Emphasis_Command a) = toElem a
    toElem (Emphasis_Computeroutput a) = toElem a
    toElem (Emphasis_Email a) = toElem a
    toElem (Emphasis_Filename a) = toElem a
    toElem (Emphasis_Literal a) = toElem a
    toElem (Emphasis_Option a) = toElem a
    toElem (Emphasis_Replaceable a) = toElem a
    toElem (Emphasis_Systemitem a) = toElem a
    toElem (Emphasis_Userinput a) = toElem a
    toElem (Emphasis_Inlinemediaobject a) = toElem a
instance XmlAttrType Emphasis_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Emphasis_Revisionflag_Changed
	    translate "added" = Just Emphasis_Revisionflag_Added
	    translate "deleted" = Just Emphasis_Revisionflag_Deleted
	    translate "off" = Just Emphasis_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Emphasis_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Emphasis_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Emphasis_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Emphasis_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Phrase where
    fromElem (CElem (Elem "phrase" as c0):rest) =
	(\(a,ca)->
	   (Just (Phrase (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Phrase as a) =
	[CElem (Elem "phrase" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Phrase_Attrs where
    fromAttrs as =
	Phrase_Attrs
	  { phraseId = possibleA fromAttrToStr "id" as
	  , phraseLang = possibleA fromAttrToStr "lang" as
	  , phraseRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , phraseRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (phraseId v)
	, maybeToAttr toAttrFrStr "lang" (phraseLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (phraseRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (phraseRole v)
	]
instance XmlContent Phrase_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Phrase_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Phrase_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Phrase_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Phrase_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Phrase_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Phrase_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Phrase_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Phrase_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Phrase_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Phrase_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Phrase_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Phrase_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Phrase_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Phrase_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Phrase_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Phrase_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Phrase_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Phrase_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Phrase_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Phrase_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Phrase_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Phrase_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Phrase_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Phrase_Str a) = toText a
    toElem (Phrase_Footnoteref a) = toElem a
    toElem (Phrase_Xref a) = toElem a
    toElem (Phrase_Abbrev a) = toElem a
    toElem (Phrase_Acronym a) = toElem a
    toElem (Phrase_Citetitle a) = toElem a
    toElem (Phrase_Emphasis a) = toElem a
    toElem (Phrase_Footnote a) = toElem a
    toElem (Phrase_Phrase a) = toElem a
    toElem (Phrase_Quote a) = toElem a
    toElem (Phrase_Trademark a) = toElem a
    toElem (Phrase_Link a) = toElem a
    toElem (Phrase_Ulink a) = toElem a
    toElem (Phrase_Command a) = toElem a
    toElem (Phrase_Computeroutput a) = toElem a
    toElem (Phrase_Email a) = toElem a
    toElem (Phrase_Filename a) = toElem a
    toElem (Phrase_Literal a) = toElem a
    toElem (Phrase_Option a) = toElem a
    toElem (Phrase_Replaceable a) = toElem a
    toElem (Phrase_Systemitem a) = toElem a
    toElem (Phrase_Userinput a) = toElem a
    toElem (Phrase_Inlinemediaobject a) = toElem a
instance XmlAttrType Phrase_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Phrase_Revisionflag_Changed
	    translate "added" = Just Phrase_Revisionflag_Added
	    translate "deleted" = Just Phrase_Revisionflag_Deleted
	    translate "off" = Just Phrase_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Phrase_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Phrase_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Phrase_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Phrase_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Quote where
    fromElem (CElem (Elem "quote" as c0):rest) =
	(\(a,ca)->
	   (Just (Quote (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Quote as a) =
	[CElem (Elem "quote" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Quote_Attrs where
    fromAttrs as =
	Quote_Attrs
	  { quoteId = possibleA fromAttrToStr "id" as
	  , quoteLang = possibleA fromAttrToStr "lang" as
	  , quoteRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , quoteRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (quoteId v)
	, maybeToAttr toAttrFrStr "lang" (quoteLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (quoteRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (quoteRole v)
	]
instance XmlContent Quote_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Quote_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Quote_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Quote_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Quote_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Quote_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Quote_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Quote_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Quote_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Quote_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Quote_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Quote_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Quote_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Quote_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Quote_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Quote_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Quote_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Quote_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Quote_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Quote_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Quote_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Quote_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Quote_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Quote_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Quote_Str a) = toText a
    toElem (Quote_Footnoteref a) = toElem a
    toElem (Quote_Xref a) = toElem a
    toElem (Quote_Abbrev a) = toElem a
    toElem (Quote_Acronym a) = toElem a
    toElem (Quote_Citetitle a) = toElem a
    toElem (Quote_Emphasis a) = toElem a
    toElem (Quote_Footnote a) = toElem a
    toElem (Quote_Phrase a) = toElem a
    toElem (Quote_Quote a) = toElem a
    toElem (Quote_Trademark a) = toElem a
    toElem (Quote_Link a) = toElem a
    toElem (Quote_Ulink a) = toElem a
    toElem (Quote_Command a) = toElem a
    toElem (Quote_Computeroutput a) = toElem a
    toElem (Quote_Email a) = toElem a
    toElem (Quote_Filename a) = toElem a
    toElem (Quote_Literal a) = toElem a
    toElem (Quote_Option a) = toElem a
    toElem (Quote_Replaceable a) = toElem a
    toElem (Quote_Systemitem a) = toElem a
    toElem (Quote_Userinput a) = toElem a
    toElem (Quote_Inlinemediaobject a) = toElem a
instance XmlAttrType Quote_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Quote_Revisionflag_Changed
	    translate "added" = Just Quote_Revisionflag_Added
	    translate "deleted" = Just Quote_Revisionflag_Deleted
	    translate "off" = Just Quote_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Quote_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Quote_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Quote_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Quote_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Trademark where
    fromElem (CElem (Elem "trademark" as c0):rest) =
	(\(a,ca)->
	   (Just (Trademark (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Trademark as a) =
	[CElem (Elem "trademark" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Trademark_Attrs where
    fromAttrs as =
	Trademark_Attrs
	  { trademarkClass = defaultA fromAttrToTyp Trademark_Class_Trade "class" as
	  , trademarkId = possibleA fromAttrToStr "id" as
	  , trademarkLang = possibleA fromAttrToStr "lang" as
	  , trademarkRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , trademarkRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ defaultToAttr toAttrFrTyp "class" (trademarkClass v)
	, maybeToAttr toAttrFrStr "id" (trademarkId v)
	, maybeToAttr toAttrFrStr "lang" (trademarkLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (trademarkRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (trademarkRole v)
	]
instance XmlContent Trademark_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Trademark_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Trademark_Link a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Trademark_Ulink a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Trademark_Command a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Trademark_Computeroutput a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Trademark_Email a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Trademark_Filename a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Trademark_Literal a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Trademark_Option a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Trademark_Replaceable a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Trademark_Systemitem a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Trademark_Userinput a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Trademark_Inlinemediaobject a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Trademark_Emphasis a), rest)
														(_,_) ->
														    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Trademark_Str a) = toText a
    toElem (Trademark_Link a) = toElem a
    toElem (Trademark_Ulink a) = toElem a
    toElem (Trademark_Command a) = toElem a
    toElem (Trademark_Computeroutput a) = toElem a
    toElem (Trademark_Email a) = toElem a
    toElem (Trademark_Filename a) = toElem a
    toElem (Trademark_Literal a) = toElem a
    toElem (Trademark_Option a) = toElem a
    toElem (Trademark_Replaceable a) = toElem a
    toElem (Trademark_Systemitem a) = toElem a
    toElem (Trademark_Userinput a) = toElem a
    toElem (Trademark_Inlinemediaobject a) = toElem a
    toElem (Trademark_Emphasis a) = toElem a
instance XmlAttrType Trademark_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "service" = Just Trademark_Class_Service
	    translate "trade" = Just Trademark_Class_Trade
	    translate "registered" = Just Trademark_Class_Registered
	    translate "copyright" = Just Trademark_Class_Copyright
	    translate _ = Nothing
    toAttrFrTyp n Trademark_Class_Service = Just (n, str2attr "service")
    toAttrFrTyp n Trademark_Class_Trade = Just (n, str2attr "trade")
    toAttrFrTyp n Trademark_Class_Registered = Just (n, str2attr "registered")
    toAttrFrTyp n Trademark_Class_Copyright = Just (n, str2attr "copyright")
instance XmlAttrType Trademark_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Trademark_Revisionflag_Changed
	    translate "added" = Just Trademark_Revisionflag_Added
	    translate "deleted" = Just Trademark_Revisionflag_Deleted
	    translate "off" = Just Trademark_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Trademark_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Trademark_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Trademark_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Trademark_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Link where
    fromElem (CElem (Elem "link" as c0):rest) =
	(\(a,ca)->
	   (Just (Link (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Link as a) =
	[CElem (Elem "link" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Link_Attrs where
    fromAttrs as =
	Link_Attrs
	  { linkEndterm = possibleA fromAttrToStr "endterm" as
	  , linkLinkend = definiteA fromAttrToStr "link" "linkend" as
	  , linkType = possibleA fromAttrToStr "type" as
	  , linkId = possibleA fromAttrToStr "id" as
	  , linkLang = possibleA fromAttrToStr "lang" as
	  , linkRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , linkRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "endterm" (linkEndterm v)
	, toAttrFrStr "linkend" (linkLinkend v)
	, maybeToAttr toAttrFrStr "type" (linkType v)
	, maybeToAttr toAttrFrStr "id" (linkId v)
	, maybeToAttr toAttrFrStr "lang" (linkLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (linkRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (linkRole v)
	]
instance XmlContent Link_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Link_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Link_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Link_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Link_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Link_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Link_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Link_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Link_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Link_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Link_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Link_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Link_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Link_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Link_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Link_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Link_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Link_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Link_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Link_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Link_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Link_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Link_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Link_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Link_Str a) = toText a
    toElem (Link_Footnoteref a) = toElem a
    toElem (Link_Xref a) = toElem a
    toElem (Link_Abbrev a) = toElem a
    toElem (Link_Acronym a) = toElem a
    toElem (Link_Citetitle a) = toElem a
    toElem (Link_Emphasis a) = toElem a
    toElem (Link_Footnote a) = toElem a
    toElem (Link_Phrase a) = toElem a
    toElem (Link_Quote a) = toElem a
    toElem (Link_Trademark a) = toElem a
    toElem (Link_Link a) = toElem a
    toElem (Link_Ulink a) = toElem a
    toElem (Link_Command a) = toElem a
    toElem (Link_Computeroutput a) = toElem a
    toElem (Link_Email a) = toElem a
    toElem (Link_Filename a) = toElem a
    toElem (Link_Literal a) = toElem a
    toElem (Link_Option a) = toElem a
    toElem (Link_Replaceable a) = toElem a
    toElem (Link_Systemitem a) = toElem a
    toElem (Link_Userinput a) = toElem a
    toElem (Link_Inlinemediaobject a) = toElem a
instance XmlAttrType Link_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Link_Revisionflag_Changed
	    translate "added" = Just Link_Revisionflag_Added
	    translate "deleted" = Just Link_Revisionflag_Deleted
	    translate "off" = Just Link_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Link_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Link_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Link_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Link_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Ulink where
    fromElem (CElem (Elem "ulink" as c0):rest) =
	(\(a,ca)->
	   (Just (Ulink (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Ulink as a) =
	[CElem (Elem "ulink" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Ulink_Attrs where
    fromAttrs as =
	Ulink_Attrs
	  { ulinkUrl = definiteA fromAttrToStr "ulink" "url" as
	  , ulinkType = possibleA fromAttrToStr "type" as
	  , ulinkId = possibleA fromAttrToStr "id" as
	  , ulinkLang = possibleA fromAttrToStr "lang" as
	  , ulinkRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , ulinkRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "url" (ulinkUrl v)
	, maybeToAttr toAttrFrStr "type" (ulinkType v)
	, maybeToAttr toAttrFrStr "id" (ulinkId v)
	, maybeToAttr toAttrFrStr "lang" (ulinkLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (ulinkRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (ulinkRole v)
	]
instance XmlContent Ulink_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Ulink_Str a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Ulink_Footnoteref a), rest)
		(_,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Ulink_Xref a), rest)
			(_,_) ->
				case (fromElem c0) of
				(Just a,rest) -> (Just (Ulink_Abbrev a), rest)
				(_,_) ->
					case (fromElem c0) of
					(Just a,rest) -> (Just (Ulink_Acronym a), rest)
					(_,_) ->
						case (fromElem c0) of
						(Just a,rest) -> (Just (Ulink_Citetitle a), rest)
						(_,_) ->
							case (fromElem c0) of
							(Just a,rest) -> (Just (Ulink_Emphasis a), rest)
							(_,_) ->
								case (fromElem c0) of
								(Just a,rest) -> (Just (Ulink_Footnote a), rest)
								(_,_) ->
									case (fromElem c0) of
									(Just a,rest) -> (Just (Ulink_Phrase a), rest)
									(_,_) ->
										case (fromElem c0) of
										(Just a,rest) -> (Just (Ulink_Quote a), rest)
										(_,_) ->
											case (fromElem c0) of
											(Just a,rest) -> (Just (Ulink_Trademark a), rest)
											(_,_) ->
												case (fromElem c0) of
												(Just a,rest) -> (Just (Ulink_Link a), rest)
												(_,_) ->
													case (fromElem c0) of
													(Just a,rest) -> (Just (Ulink_Ulink a), rest)
													(_,_) ->
														case (fromElem c0) of
														(Just a,rest) -> (Just (Ulink_Command a), rest)
														(_,_) ->
															case (fromElem c0) of
															(Just a,rest) -> (Just (Ulink_Computeroutput a), rest)
															(_,_) ->
																case (fromElem c0) of
																(Just a,rest) -> (Just (Ulink_Email a), rest)
																(_,_) ->
																	case (fromElem c0) of
																	(Just a,rest) -> (Just (Ulink_Filename a), rest)
																	(_,_) ->
																		case (fromElem c0) of
																		(Just a,rest) -> (Just (Ulink_Literal a), rest)
																		(_,_) ->
																			case (fromElem c0) of
																			(Just a,rest) -> (Just (Ulink_Option a), rest)
																			(_,_) ->
																				case (fromElem c0) of
																				(Just a,rest) -> (Just (Ulink_Replaceable a), rest)
																				(_,_) ->
																					case (fromElem c0) of
																					(Just a,rest) -> (Just (Ulink_Systemitem a), rest)
																					(_,_) ->
																						case (fromElem c0) of
																						(Just a,rest) -> (Just (Ulink_Userinput a), rest)
																						(_,_) ->
																							case (fromElem c0) of
																							(Just a,rest) -> (Just (Ulink_Inlinemediaobject a), rest)
																							(_,_) ->
																							    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Ulink_Str a) = toText a
    toElem (Ulink_Footnoteref a) = toElem a
    toElem (Ulink_Xref a) = toElem a
    toElem (Ulink_Abbrev a) = toElem a
    toElem (Ulink_Acronym a) = toElem a
    toElem (Ulink_Citetitle a) = toElem a
    toElem (Ulink_Emphasis a) = toElem a
    toElem (Ulink_Footnote a) = toElem a
    toElem (Ulink_Phrase a) = toElem a
    toElem (Ulink_Quote a) = toElem a
    toElem (Ulink_Trademark a) = toElem a
    toElem (Ulink_Link a) = toElem a
    toElem (Ulink_Ulink a) = toElem a
    toElem (Ulink_Command a) = toElem a
    toElem (Ulink_Computeroutput a) = toElem a
    toElem (Ulink_Email a) = toElem a
    toElem (Ulink_Filename a) = toElem a
    toElem (Ulink_Literal a) = toElem a
    toElem (Ulink_Option a) = toElem a
    toElem (Ulink_Replaceable a) = toElem a
    toElem (Ulink_Systemitem a) = toElem a
    toElem (Ulink_Userinput a) = toElem a
    toElem (Ulink_Inlinemediaobject a) = toElem a
instance XmlAttrType Ulink_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Ulink_Revisionflag_Changed
	    translate "added" = Just Ulink_Revisionflag_Added
	    translate "deleted" = Just Ulink_Revisionflag_Deleted
	    translate "off" = Just Ulink_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Ulink_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Ulink_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Ulink_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Ulink_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Footnoteref where
    fromElem (CElem (Elem "footnoteref" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "footnoteref" (toAttrs as) [])]
instance XmlAttributes Footnoteref where
    fromAttrs as =
	Footnoteref
	  { footnoterefLinkend = definiteA fromAttrToStr "footnoteref" "linkend" as
	  , footnoterefLabel = possibleA fromAttrToStr "label" as
	  , footnoterefId = possibleA fromAttrToStr "id" as
	  , footnoterefLang = possibleA fromAttrToStr "lang" as
	  , footnoterefRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , footnoterefRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "linkend" (footnoterefLinkend v)
	, maybeToAttr toAttrFrStr "label" (footnoterefLabel v)
	, maybeToAttr toAttrFrStr "id" (footnoterefId v)
	, maybeToAttr toAttrFrStr "lang" (footnoterefLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (footnoterefRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (footnoterefRole v)
	]
instance XmlAttrType Footnoteref_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Footnoteref_Revisionflag_Changed
	    translate "added" = Just Footnoteref_Revisionflag_Added
	    translate "deleted" = Just Footnoteref_Revisionflag_Deleted
	    translate "off" = Just Footnoteref_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Footnoteref_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Footnoteref_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Footnoteref_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Footnoteref_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Xref where
    fromElem (CElem (Elem "xref" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "xref" (toAttrs as) [])]
instance XmlAttributes Xref where
    fromAttrs as =
	Xref
	  { xrefEndterm = possibleA fromAttrToStr "endterm" as
	  , xrefLinkend = definiteA fromAttrToStr "xref" "linkend" as
	  , xrefId = possibleA fromAttrToStr "id" as
	  , xrefLang = possibleA fromAttrToStr "lang" as
	  , xrefRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , xrefRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "endterm" (xrefEndterm v)
	, toAttrFrStr "linkend" (xrefLinkend v)
	, maybeToAttr toAttrFrStr "id" (xrefId v)
	, maybeToAttr toAttrFrStr "lang" (xrefLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (xrefRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (xrefRole v)
	]
instance XmlAttrType Xref_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Xref_Revisionflag_Changed
	    translate "added" = Just Xref_Revisionflag_Added
	    translate "deleted" = Just Xref_Revisionflag_Deleted
	    translate "off" = Just Xref_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Xref_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Xref_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Xref_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Xref_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Appendix where
    fromElem (CElem (Elem "appendix" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (Just (Appendix (fromAttrs as) a b c d), rest))
		 (definite fromElem "OneOf" "appendix" cc))
	      (fromElem cb))
	   (fromElem ca))
	(definite fromElem "<title>" "appendix" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Appendix as a b c d) =
	[CElem (Elem "appendix" (toAttrs as) (toElem a ++
					      maybe [] toElem b ++
					      maybe [] toElem c ++ toElem d))]
instance XmlAttributes Appendix_Attrs where
    fromAttrs as =
	Appendix_Attrs
	  { appendixLabel = possibleA fromAttrToStr "label" as
	  , appendixStatus = possibleA fromAttrToStr "status" as
	  , appendixId = possibleA fromAttrToStr "id" as
	  , appendixLang = possibleA fromAttrToStr "lang" as
	  , appendixRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , appendixRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (appendixLabel v)
	, maybeToAttr toAttrFrStr "status" (appendixStatus v)
	, maybeToAttr toAttrFrStr "id" (appendixId v)
	, maybeToAttr toAttrFrStr "lang" (appendixLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (appendixRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (appendixRole v)
	]
instance XmlAttrType Appendix_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Appendix_Revisionflag_Changed
	    translate "added" = Just Appendix_Revisionflag_Added
	    translate "deleted" = Just Appendix_Revisionflag_Deleted
	    translate "off" = Just Appendix_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Appendix_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Appendix_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Appendix_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Appendix_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Section where
    fromElem (CElem (Elem "section" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (Just (Section (fromAttrs as) a b c d e), rest))
		    (definite fromElem "OneOf" "section" cd))
		 (fromElem cc))
	      (fromElem cb))
	   (definite fromElem "<title>" "section" ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Section as a b c d e) =
	[CElem (Elem "section" (toAttrs as) (maybe [] toElem a
					     ++ toElem b ++ maybe [] toElem c ++
					     maybe [] toElem d ++ toElem e))]
instance XmlAttributes Section_Attrs where
    fromAttrs as =
	Section_Attrs
	  { sectionLabel = possibleA fromAttrToStr "label" as
	  , sectionStatus = possibleA fromAttrToStr "status" as
	  , sectionId = possibleA fromAttrToStr "id" as
	  , sectionLang = possibleA fromAttrToStr "lang" as
	  , sectionRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , sectionRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "label" (sectionLabel v)
	, maybeToAttr toAttrFrStr "status" (sectionStatus v)
	, maybeToAttr toAttrFrStr "id" (sectionId v)
	, maybeToAttr toAttrFrStr "lang" (sectionLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (sectionRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (sectionRole v)
	]
instance XmlAttrType Section_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Section_Revisionflag_Changed
	    translate "added" = Just Section_Revisionflag_Added
	    translate "deleted" = Just Section_Revisionflag_Deleted
	    translate "off" = Just Section_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Section_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Section_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Section_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Section_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Bibliography where
    fromElem (CElem (Elem "bibliography" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Bibliography (fromAttrs as) a b c), rest))
	      (definite fromElem "OneOf" "bibliography" cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliography as a b c) =
	[CElem (Elem "bibliography" (toAttrs as) (maybe [] toElem a
						  ++ concatMap toElem b ++
						  toElem c))]
instance XmlAttributes Bibliography_Attrs where
    fromAttrs as =
	Bibliography_Attrs
	  { bibliographyStatus = possibleA fromAttrToStr "status" as
	  , bibliographyId = possibleA fromAttrToStr "id" as
	  , bibliographyLang = possibleA fromAttrToStr "lang" as
	  , bibliographyRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , bibliographyRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "status" (bibliographyStatus v)
	, maybeToAttr toAttrFrStr "id" (bibliographyId v)
	, maybeToAttr toAttrFrStr "lang" (bibliographyLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (bibliographyRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (bibliographyRole v)
	]
instance XmlAttrType Bibliography_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Bibliography_Revisionflag_Changed
	    translate "added" = Just Bibliography_Revisionflag_Added
	    translate "deleted" = Just Bibliography_Revisionflag_Deleted
	    translate "off" = Just Bibliography_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Bibliography_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Bibliography_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Bibliography_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Bibliography_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Bibliodiv where
    fromElem (CElem (Elem "bibliodiv" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Bibliodiv (fromAttrs as) a b c), rest))
	      (definite fromElem "(bibliomixed)+" "bibliodiv" cb))
	   (many fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bibliodiv as a b c) =
	[CElem (Elem "bibliodiv" (toAttrs as) (maybe [] toElem a
					       ++ concatMap toElem b ++
					       toElem c))]
instance XmlAttributes Bibliodiv_Attrs where
    fromAttrs as =
	Bibliodiv_Attrs
	  { bibliodivStatus = possibleA fromAttrToStr "status" as
	  , bibliodivId = possibleA fromAttrToStr "id" as
	  , bibliodivLang = possibleA fromAttrToStr "lang" as
	  , bibliodivRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , bibliodivRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "status" (bibliodivStatus v)
	, maybeToAttr toAttrFrStr "id" (bibliodivId v)
	, maybeToAttr toAttrFrStr "lang" (bibliodivLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (bibliodivRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (bibliodivRole v)
	]
instance XmlAttrType Bibliodiv_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Bibliodiv_Revisionflag_Changed
	    translate "added" = Just Bibliodiv_Revisionflag_Added
	    translate "deleted" = Just Bibliodiv_Revisionflag_Deleted
	    translate "off" = Just Bibliodiv_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Bibliodiv_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Bibliodiv_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Bibliodiv_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Bibliodiv_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Article where
    fromElem (CElem (Elem "article" as c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (Just (Article (fromAttrs as) a b c d), rest))
		 (many fromElem cc))
	      (definite fromElem "OneOf" "article" cb))
	   (fromElem ca))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Article as a b c d) =
	[CElem (Elem "article" (toAttrs as) (maybe [] toElem a
					     ++ maybe [] toElem b ++ toElem c ++
					     concatMap toElem d))]
instance XmlAttributes Article_Attrs where
    fromAttrs as =
	Article_Attrs
	  { articleClass = possibleA fromAttrToTyp "class" as
	  , articleParentbook = possibleA fromAttrToStr "parentbook" as
	  , articleStatus = possibleA fromAttrToStr "status" as
	  , articleId = possibleA fromAttrToStr "id" as
	  , articleLang = possibleA fromAttrToStr "lang" as
	  , articleRevisionflag = possibleA fromAttrToTyp "revisionflag" as
	  , articleRole = possibleA fromAttrToStr "role" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrTyp "class" (articleClass v)
	, maybeToAttr toAttrFrStr "parentbook" (articleParentbook v)
	, maybeToAttr toAttrFrStr "status" (articleStatus v)
	, maybeToAttr toAttrFrStr "id" (articleId v)
	, maybeToAttr toAttrFrStr "lang" (articleLang v)
	, maybeToAttr toAttrFrTyp "revisionflag" (articleRevisionflag v)
	, maybeToAttr toAttrFrStr "role" (articleRole v)
	]
instance XmlAttrType Article_Class where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "journalarticle" = Just Article_Class_Journalarticle
	    translate "productsheet" = Just Article_Class_Productsheet
	    translate "whitepaper" = Just Article_Class_Whitepaper
	    translate "techreport" = Just Article_Class_Techreport
	    translate "specification" = Just Article_Class_Specification
	    translate "faq" = Just Article_Class_Faq
	    translate _ = Nothing
    toAttrFrTyp n Article_Class_Journalarticle = Just (n, str2attr "journalarticle")
    toAttrFrTyp n Article_Class_Productsheet = Just (n, str2attr "productsheet")
    toAttrFrTyp n Article_Class_Whitepaper = Just (n, str2attr "whitepaper")
    toAttrFrTyp n Article_Class_Techreport = Just (n, str2attr "techreport")
    toAttrFrTyp n Article_Class_Specification = Just (n, str2attr "specification")
    toAttrFrTyp n Article_Class_Faq = Just (n, str2attr "faq")
instance XmlAttrType Article_Revisionflag where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "changed" = Just Article_Revisionflag_Changed
	    translate "added" = Just Article_Revisionflag_Added
	    translate "deleted" = Just Article_Revisionflag_Deleted
	    translate "off" = Just Article_Revisionflag_Off
	    translate _ = Nothing
    toAttrFrTyp n Article_Revisionflag_Changed = Just (n, str2attr "changed")
    toAttrFrTyp n Article_Revisionflag_Added = Just (n, str2attr "added")
    toAttrFrTyp n Article_Revisionflag_Deleted = Just (n, str2attr "deleted")
    toAttrFrTyp n Article_Revisionflag_Off = Just (n, str2attr "off")
instance XmlContent Sectioninfo where
    fromElem (CElem (Elem "sectioninfo" [] c0):rest) =
	(\(a,ca)->
	   (Just (Sectioninfo a), rest))
	(definite fromElem "(mediaobject|legalnotice|keywordset|subjectset|abbrev|abstract|author|authorgroup|bibliomisc|copyright|corpauthor|date|edition|editor|issuenum|othercredit|pubdate|publishername|releaseinfo|revhistory|subtitle|title|titleabbrev|volumenum|citetitle|honorific|firstname|surname|lineage|othername|affiliation|authorblurb)+" "sectioninfo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Sectioninfo a) =
	[CElem (Elem "sectioninfo" [] (toElem a))]


{-Done-}

data OneOf32 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af
    = OneOf32 a | TwoOf32 b | ThreeOf32 c | FourOf32 d | FiveOf32 e
    | SixOf32 f | SevenOf32 g | EightOf32 h | NineOf32 i | TenOf32 j
    | ElevenOf32 k | TwelveOf32 l | ThirteenOf32 m | FourteenOf32 n
    | FifteenOf32 o | SixteenOf32 p | SeventeenOf32 q | EighteenOf32 r
    | NineteenOf32 s | TwentyOf32 t | Choice21Of32 u | Choice22Of32 v
    | Choice23Of32 w | Choice24Of32 x | Choice25Of32 y | Choice26Of32 z
    | Choice27Of32 aa | Choice28Of32 ab | Choice29Of32 ac | Choice30Of32 ad
    | Choice31Of32 ae | Choice32Of32 af
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af)
    => XmlContent (OneOf32 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af)
  where
    fromElem cs =
        (choice OneOf32 $ choice TwoOf32 $ choice ThreeOf32 $ choice FourOf32
        $ choice FiveOf32 $ choice SixOf32 $ choice SevenOf32
        $ choice EightOf32 $ choice NineOf32 $ choice TenOf32
        $ choice ElevenOf32 $ choice TwelveOf32 $ choice ThirteenOf32
        $ choice FourteenOf32 $ choice FifteenOf32 $ choice SixteenOf32
        $ choice SeventeenOf32 $ choice EighteenOf32 $ choice NineteenOf32
        $ choice TwentyOf32 $ choice Choice21Of32 $ choice Choice22Of32
        $ choice Choice23Of32 $ choice Choice24Of32 $ choice Choice25Of32
        $ choice Choice26Of32 $ choice Choice27Of32 $ choice Choice28Of32
        $ choice Choice29Of32 $ choice Choice30Of32 $ choice Choice31Of32
        $ choice Choice32Of32
        $ (\c->(Nothing,c))) cs
    toElem (OneOf32 x) = toElem x
    toElem (TwoOf32 x) = toElem x
    toElem (ThreeOf32 x) = toElem x
    toElem (FourOf32 x) = toElem x
    toElem (FiveOf32 x) = toElem x
    toElem (SixOf32 x) = toElem x
    toElem (SevenOf32 x) = toElem x
    toElem (EightOf32 x) = toElem x
    toElem (NineOf32 x) = toElem x
    toElem (TenOf32 x) = toElem x
    toElem (ElevenOf32 x) = toElem x
    toElem (TwelveOf32 x) = toElem x
    toElem (ThirteenOf32 x) = toElem x
    toElem (FourteenOf32 x) = toElem x
    toElem (FifteenOf32 x) = toElem x
    toElem (SixteenOf32 x) = toElem x
    toElem (SeventeenOf32 x) = toElem x
    toElem (EighteenOf32 x) = toElem x
    toElem (NineteenOf32 x) = toElem x
    toElem (TwentyOf32 x) = toElem x
    toElem (Choice21Of32 x) = toElem x
    toElem (Choice22Of32 x) = toElem x
    toElem (Choice23Of32 x) = toElem x
    toElem (Choice24Of32 x) = toElem x
    toElem (Choice25Of32 x) = toElem x
    toElem (Choice26Of32 x) = toElem x
    toElem (Choice27Of32 x) = toElem x
    toElem (Choice28Of32 x) = toElem x
    toElem (Choice29Of32 x) = toElem x
    toElem (Choice30Of32 x) = toElem x
    toElem (Choice31Of32 x) = toElem x
    toElem (Choice32Of32 x) = toElem x

----
