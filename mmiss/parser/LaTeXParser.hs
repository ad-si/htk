module LaTeXParser (
   -- new interface.  For now, just a make-weight bolted on top of the old one.
   PackageId(..),

   parseMMiSSLatex, 
      -- :: FileSystem -> FilePath -> Bool
      -- -> IO (WithError (Element,[(MMiSSLatexPreamble,PackageId)]))
      -- Parses the given File (by means of the FileSystem functions given) and
      -- returns the corresponding XML-Element and (if the boolean parameter
      -- is true) the list of Preambles found in the mmisslatex file. If the bool
      -- parameter is false, it will throw away anything which preceeds the first
      -- mmiss environent found.
   
   makeMMiSSLatexContent,
      -- :: Element -> Bool 
      -- -> [(MMiSSLatexPreamble,PackageId)]
      -- -> WithError (EmacsContent ((String,Char),[Attribute])) 
      -- This is used for Emacs and also for other consumers of LaTeX text
      -- expecting a single file, for example the MMiSS checker and the
      -- XML API.
      -- The bool parameter controls whether MMiSS 'includeXXX' elements are expanded
      -- into Emacs-Links (False) or into \includeXXX commands when the output is designated
      -- to be feed into latex (True). In the latter case, the preambles are merged and
      -- included in the output, otherwise, they are left out.

   writeMMiSSLatex, 
      -- :: FileSystem -> Element -> Bool
      -- -> [(MMiSSLatexPreamble,PackageId)]
      -- -> IO (WithError ()) 
      -- This is used for exporting the LaTeX text to a directory within
      -- a file system, and may split up the element.

   mkLaTeXString, 
      -- :: EmacsContent ((String,Char),[Attribute]) -> String
      -- Convert the contents of an Emacs buffer representing a particular
      -- LaTeX file into a String.

   -- OLD INTERFACE
   makeMMiSSLatex,
   -- :: (Element, Bool, [(MMiSSLatexPreamble,[MMiSSExtraPreambleData])]) 
   --   -> WithError (EmacsContent ((String, Char), [Attribute]))
   -- Turns an Element into a MMiSSLaTeX source
   -- If the Bool is set, attaches a preamble.
   -- MMiSSExtraPreambleData contains extra information about the call-site
   -- of the element containing a preamble.

   classifyLabelledTag, -- :: String -> Maybe Char
   -- Maps an Xml tag to its corresponding mini-type if it has one.
   -- (The mini-type is just something that identifies what sort of
   -- include the String needs.)

   fromIncludeStr, --  String -> Char
   fromIncludeStrOpt, -- String -> Maybe Char
   toIncludeStr, -- Char -> String
   -- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
   -- the corresponding includeXXX command.
   mapLabelledTag,
   )
 where

-- module LaTeXParser where

import IO(FilePath)

import List
import Char
import Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.Pretty as PP
import Text.PrettyPrint.HughesPJ hiding (char,space)
import Text.XML.HaXml.Combinators hiding (find)

import LaTeXParserCore
import LaTeXPreamble
-- import Dynamics
import Computation hiding (try)
import ExtendedPrelude(unsplitByChar,mapEq)
import EmacsContent
import EntityNames
import AtomString
import CodedValue
import FileNames
import QuickReadShow
-- import EmacsEdit(TypedName)


-- ---------------------------------------------------------------------------
-- New Interface
-- ---------------------------------------------------------------------------

-- types
newtype PackageId = PackageId {packageIdStr :: String} deriving (Eq,Ord)

-- Functions
parseMMiSSLatex :: FileSystem -> FilePath -> Bool
                   -> IO (WithError (Element,[(MMiSSLatexPreamble,PackageId)]))
parseMMiSSLatex fileSystem filePath searchPreamble =
   do
      strWE <- readString fileSystem filePath 
      case fromWithError strWE of
         Left err -> return (fail err)
         Right str ->
            do let result = parseFrags str
               case result of
		  Left err -> return (hasError (show err))
		  Right fs  ->
                    do preEl <- extractPreamble fileSystem filePath fs
		       case fromWithError preEl of
			 Left err -> return(hasError(err))
			 Right (preambleOpt, rootFrag) -> 
			   do 
			     aList <- mapM (expandInputs fileSystem filePath) [rootFrag]  -- IO [WithError([Frag])]
			     parsedWE <- return(listWithError aList) -- WithError [[Frag]]   
			     case fromWithError parsedWE of
			       Left err -> return (fail err)
			       Right newFrags  -> 
				 let xmlWE = makeXML (head (concat newFrags))
				 in case fromWithError xmlWE of
				   Left err -> return (fail err)
				   Right (el @ (Elem _ atts _)) ->
				      do
					 let
					    packageId = PackageId (getParam "packageId" atts)
					    preambleList = case preambleOpt of
					       Nothing -> []
					       Just preamble -> [(preamble,packageId)]
					 return (hasValue (el,preambleList))

  where 
    -- expandInputs guckt noch nicht rekursiv in aufgelöste inputs rein
    --
    expandInputs :: FileSystem -> FilePath -> Frag -> IO ( WithError([Frag]) )
    expandInputs fileSystem filePath f =
      case f of
        (Env id ps contentFrags) -> 
           do aList <- mapM (expandInputs fileSystem filePath) contentFrags -- IO [WithError([Frag])]
              parsedWE <- return(listWithError aList) -- WithError [[Frag]]   
              case fromWithError parsedWE of
	        Left err -> return (fail err)
		Right newFrags  ->  
                  return (hasValue([(Env id ps (concat newFrags))]))
        (Command "input" (LParams sps _ _ _)) -> 
           do let fstr = singleParamToString(head sps)
                  filename = delete '{' (delete '}' fstr)          
              strWE <- readString fileSystem (createInputPath filePath filename)
	      case fromWithError strWE of 
		Left err -> return (fail (err ++ " File: " ++ (createInputPath filePath filename)))
		Right str -> 
                  case findInString str mmissNoParsingPragma of
                       -- True means that the file should not be parsed, because the correspondig pragma
                       -- has been found in the file:
		     True -> return(hasValue([f]))
		     False ->
		       let result = parseFrags str      
		       in case result of
			    Left err -> return (hasError ("in File " ++ (createInputPath filePath filename) 
                                                          ++ " " ++ (show err)))
			    Right newfs -> 
			      let specialFrag1 = Special InputStart (makeTextElem [f] "")
				  specialFrag2 = Special InputEnd (filename ++ "\n")
			      in return (hasValue ([specialFrag1] ++ newfs ++ [specialFrag2]))
        (Command "include" (LParams sps _ _ _)) -> 
           do let fstr = singleParamToString(head sps)
                  filename = delete '{' (delete '}' fstr)          
              strWE <- readString fileSystem (createInputPath filePath filename)
	      case fromWithError strWE of 
		Left err -> return (fail (err ++ " File: " ++ (createInputPath filePath filename)))
		Right str -> 
                  case findInString str mmissNoParsingPragma of
                       -- True means that the file should not be parsed, because the corresponding pragma
                       -- has been found in the file:
		     True -> return(hasValue([f]))
		     False ->
		       let result = parseFrags str      
		       in case result of
			    Left err -> return (hasError ("in File " ++ (createInputPath filePath filename) ++ " " ++ (show err)))
			    Right newfs ->
			      let specialFrag1 = Special InputStart (makeTextElem [f] "")
				  specialFrag2 = Special InputEnd (filename ++ "\n")
			      in return (hasValue ([specialFrag1] ++ newfs ++ [specialFrag2]))
        otherwise -> return(hasValue([f]))

    createInputPath filePath filename = 
      let (dir, _) = splitName filePath
      in case splitExtension filename of
           Nothing -> combineNames dir (filename ++ ".tex")
           Just(name, ext) -> combineNames dir filename
      

makeMMiSSLatexContent :: Element -> Bool -> [(MMiSSLatexPreamble,PackageId)]
   -> WithError (EmacsContent ((String,Char),[Attribute]))
makeMMiSSLatexContent el b preambleInfos0 =
   let
      preambleInfos1 = map
         (\ (preamble,packageId) 
            -> (preamble,error "No preamble data supplied"))
         preambleInfos0
   in
      makeMMiSSLatex (el,b,preambleInfos1)


writeMMiSSLatex :: FileSystem -> Element -> Bool ->
   [(MMiSSLatexPreamble,PackageId)]
   -> IO (WithError ())
writeMMiSSLatex fileSystem (el @ (Elem _ atts _)) b preambleInfos0 =
   do
      let
         contentWE = makeMMiSSLatexContent el b preambleInfos0
      case fromWithError contentWE of
         Left err -> return (fail err)
         Right content ->
            do
               let
                  result = mkLaTeXString content
                  label = getParam "label" atts
               writeString fileSystem result label  
 

mkLaTeXString :: EmacsContent ((String,Char),[Attribute]) -> String
mkLaTeXString (EmacsContent dataItems) =
   concatMap
      (\ dataItem -> case dataItem of
         EditableText str -> str
         EmacsLink ((included,ch),attributes) -> 
            "\\Include"
            ++ toIncludeStr ch
            ++ "{" ++ included ++ "}"
            ++ "{" ++ (getAttribs  (attributes ++ [statusAttribute]) "" ["included","status"]) ++ "}"
         )     
      dataItems

statusAttribute :: Attribute
statusAttribute = ("status",AttValue [Left "present"])


-- ---------------------------------------------------------------------------
-- Old Interface
-- ---------------------------------------------------------------------------

data Textmode = TextAllowed | NoText | TextFragment

{--
   parseImportCommands is used as fromStringWE-method in the instanciation for
   ImportCommands as StringClass. 


parseImportCommands :: String -> WithError ImportCommands

parseImportCommands s = 
  let result = parse (latexDoc []) "" s
  in case result of
       Right (Env _ _ fs)  -> 
          case (fromWithError (makeImportCmds fs [])) of
             Right pMaybe -> case pMaybe of
                               Just(p) -> hasValue(p)
                               Nothing -> hasError("Strange: makeImportCommands returns no error and no import commands.")
             Left err -> hasError(show err)
       Left err -> hasError (show err)
--}


makeXML :: Frag -> WithError (Element)
makeXML rootFrag = 
   case rootFrag of
     (Env "package" ps@(LParams _ atts _ _) fs) -> 
	case fromWithError (makeContent fs NoText "package") of 
	  Left(err) -> hasError(err)
	  Right contentList ->
	    let atts1 = map convertAttrib atts
	    in addFileAttributes (CElem (Elem "package" atts1 contentList))

     (Env name ps@(LParams _ atts _ _) fs) -> 
	case fromWithError (makeContent [(Env name ps fs)] (detectTextMode name) "Root") of
	  (Left str) -> hasError(str)
	  (Right cs) -> 
	    if ((genericLength cs) == 0) 
	      then hasError("Internal Error: no XML content could be genereated for topmost Env. '" ++ name ++ "'")
	      else case (head cs) of
		     (CElem e) -> addFileAttributes (CElem e)
		     _ -> hasError("Internal Error: no XML element could be genereated for topmost Env. '" ++ name ++ "'")                          
     otherwise -> hasError("Error in function makeXML: Topmost fragment is no environment.")
  where 
    addFileAttributes contentItem =
      let clist = (foldXml insertFileAttributes) contentItem
      in case (last clist) of
           (CElem e) -> hasValue(e)
           otherwise -> hasError("Internal Error in function 'makeXML': XML filter delivered a non-element content.") 


{----------------------------------------------------------------------------------------------------------------

  The functions in this section are used for extracting the information, which external files are addressed by the
  latex file from which the XML tree was built. The repository expects Attributes named 'files' at the elements
  containing references to external files (via the latex commands \includegraphics or \externalFile, the
  latter one is mmisslatex.
  This extraction is done by filtering the XML tree with help of the HaXml-Combinator library.
  
--}


{-- getFileCommand examines a XML content portion. If it is a processing instruction, containing a latex command
    \includegraphics or \externalFile, then it gives back the referenced filename, otherwise it
      returns nothing.
--}

getFileCommand :: Content -> [String]

getFileCommand c = 
  case c of 
    (CString True text) -> getFileCmdInternal text
    (CMisc (PI (target, str))) -> if (target == piInsertLaTeX)
                                    then getFileCmdInternal str
                                    else []
    _ -> []
  where 
     getFileCmdInternal str =
       let elFrag = parseFrags str
       in case elFrag of
            Right fs -> (foldl extractFilenames [] fs)       
            Left err -> []

     extractFilenames l c@(Command name (LParams singlePs _ _ _)) =
       if ((isPrefixOf "includegraphics" name) 
            || (isSuffixOf "includegraphics" name)
            || (name == "includeExternalFile")
            && ((length singlePs) > 0))
         then let param = singleParamToString(last singlePs)
                  fileStr = genericTake ((length param) - 2) (genericDrop 1 param)
              in (l ++ [fileStr])
         else let fs = concat (map getFragsFromSingleParam singlePs)
                  subList = foldl extractFilenames [] fs
              in (l ++ subList)   
     extractFilenames l f = l
     getFragsFromSingleParam (SingleParam fs _) = concat (map parseString fs)



{-- insertFileAttributes is a cfilter which collects the filenames from the _direct_ children of
    the given Content element and adds a 'files'-Attribute with a list of filenames referenced.
--}

insertFileAttributes :: CFilter

insertFileAttributes c@(CElem(Elem name attribs clist)) =
  let rlist = concat (map getFileCommand clist) 
      filesStr = concatFilenames rlist
      fileAttr = if (filesStr == "") 
                   then []
                   else [("files", AttValue [(Left filesStr)])]
  in [(CElem(Elem name (attribs ++ fileAttr) clist))]

insertFileAttributes c = [c]
 

concatFilenames :: [String] -> String

concatFilenames [] = ""
concatFilenames (filename:[]) = filename
concatFilenames (filename:rest) = filename ++ "," ++ concatFilenames rest



makeContent :: [Frag] -> Textmode -> String -> WithError [Content]

makeContent [] _ _ = hasValue([])
makeContent (f:frags) NoText parentEnv = 
   case f of
     (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                        in  mapWithError ([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))] ++)
                                         (makeContent frags NoText parentEnv)
     (Other str) -> if ((length (filter (not . isSpace) str) == 0) ||
                        (length (filter (not . isControl) str) == 0) ||
			((head str) == '%'))
                      -- String besteht nur aus Leerzeichen oder Zeilenenden
                      then mapWithError ([(CMisc (PI (piInsertLaTeX ,str)))] ++) (makeContent frags NoText parentEnv)
		      else hasError("No text allowed inside a " ++ parentEnv ++ "!\nString found: " ++ str)
		      -- TODO: Text, der nur aus Linefeeds besteht, muss erhalten bleiben, da er Einfluss
                      --       auf das von Latex erzeugte Layout haben kann.
     (Env name ps fs) -> 
       if (name `elem` ((map fst plainTextAtoms) ++ (map fst latexEmbeddedFormulaEnvs)))
         then hasError("Environment '" ++ name ++ 
                       "' with label '" ++ (getLabelFromParams ps) ++ 
                       "' not allowed inside a " ++ parentEnv ++ "!")
         else
           if (name == "Text")
	     then hasError("No Environment 'Text'" ++ 
                           "' with label '" ++ (getLabelFromParams ps) ++ 
                           "' allowed inside a " ++ parentEnv ++ "!")
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in myConcatWithError 
                             (cElemListWithError ename ps (makeAttribs ps name)
	                                         (makeContent fs (detectTextMode name) name))
                             (makeContent frags NoText parentEnv)
                 else  -- No MMiSS-Env. -> Put the starting and closing Kommands in a processing instruction.
                       -- Look into the environment because MMiSS-Envs could be in there: 
                   let beginDelimStr = case ps of
                                         (LParams _ _ (Just delimStr) _) -> delimStr
                                         otherwise -> ""
                       endDelimStr = case ps of
                                       (LParams _ _ _ (Just delimStr)) -> delimStr
                                       otherwise -> ""
                       begin = case name of
                                 "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
                                 "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
                                 otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
                                                ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
                       end =  case name of 
                                 "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
                                 "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
                                 otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
	   	       body = (makeContent fs NoText parentEnv)
                       whole = myConcatWithError (myConcatWithError begin body) end
                   in myConcatWithError whole (makeContent frags NoText parentEnv)
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
                   delimElem = case ps of
                                (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                                otherwise -> []
	       in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))] ++ delimElem))
                                    (makeContent frags NoText parentEnv)
          else let delimStr = case ps of
                                (LParams _ _ (Just str) _) -> str
                                otherwise -> ""
              in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
                                                                   ++ (lparamsToString ps) ++ delimStr))]))
                                            (makeContent frags NoText parentEnv)
     (Special sType str) ->
        let piStr = (show sType) ++ " " ++ str
        in mapWithError ([(CMisc (PI (piSpecial, piStr)))] ++) (makeContent frags NoText parentEnv)

makeContent (f:frags) TextAllowed parentEnv = 
  if (parentEnv `elem` (map fst listEnvs)) 
    then
      case f of
	(EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
			   in  mapWithError ([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))] ++)
					    (makeContent frags TextAllowed parentEnv)
	(Other str) -> myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX, str)))])) (makeContent frags TextAllowed parentEnv)
	(Env name ps fs) -> 
	   if (name `elem` (map fst mmiss2EnvIds))
	     then hasError("Environment '" ++ name ++ "' is not allowed in lists. Wrap it up with a \\item.")
	     else  -- No MMiSS-Env.
	       let beginDelimStr = case ps of
				     (LParams _ _ (Just delimStr) _) -> delimStr
				     otherwise -> ""
		   endDelimStr = case ps of
				  (LParams _ _ _ (Just delimStr)) -> delimStr
				  otherwise -> ""
		   begin = case name of
			     "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
			     "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
			     otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
					    ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
		   end =  case name of 
			     "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
			     "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
			     otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
		   body = (makeContent fs TextAllowed parentEnv)
		   whole = myConcatWithError (myConcatWithError begin body) end
	       in myConcatWithError whole (makeContent frags TextAllowed parentEnv)
	(Command name ps) ->
	  if (name `elem` itemNames)  
	    then let (content, restFrags) = makeListItem ps frags []
		 in myConcatWithError (hasValue([content])) (makeContent restFrags TextAllowed parentEnv)
	    else let delimStr = case ps of
				   (LParams _ _ (Just str) _) -> str
				   otherwise -> ""
		 in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
								      ++ (lparamsToString ps) ++ delimStr))]))
					       (makeContent frags TextAllowed parentEnv)
        (Special sType str) ->
          let piStr = (show sType) ++ " " ++ str
          in mapWithError ([(CMisc (PI (piSpecial, piStr)))] ++) (makeContent frags TextAllowed parentEnv)

    else   
    ------------------------------------------
    -- Parent is no List environment
    ------------------------------------------
      case f of
	(EscapedChar c) ->  let cstr = if (c == '\\') then "\\" else [c]
			    in myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))])) 
						 (makeContent frags TextAllowed parentEnv)
	(Other str) -> if ((head str) == '%')
			 then myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX ,str)))])) 
						(makeContent frags TextAllowed parentEnv)
			 else if (genericLength (filter (not . isSpace) str) > 0)
				then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
				     in  myConcatWithError (hasValue([content])) 
							   (makeContent restFrags TextAllowed parentEnv)
				else myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX, str)))])) 
						       (makeContent frags TextAllowed parentEnv)
	(Env name ps fs) -> 
	  if (name `elem` (map fst plainTextAtoms))
	    then
	      let ename = if (name `elem` (map fst latexAtomFormulaEnvs))
                            then "formula"
                            else maybe "" snd (find ((name ==) . fst) plainTextAtoms)
 		  text = makeTextElem fs ""
		  content = (hasValue([CString True text]))
		  attribs = (makeAttribs ps name)
	      in myConcatWithError
		    (cElemListWithError ename ps attribs content)
		    (makeContent frags TextAllowed parentEnv)
	    else
	      if (name == "Text")
		then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
		     in  myConcatWithError (hasValue([(makeTextFragment parentEnv ename (Just(ps)) fs [])])) 
					   (makeContent frags TextAllowed parentEnv)
		else
		  if (name `elem` (map fst mmiss2EnvIds))
		    then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
			 in myConcatWithError (cElemListWithError ename ps (makeAttribs ps name)
								  (makeContent fs (detectTextMode name) name))
					      (makeContent frags TextAllowed parentEnv)
		    else
		      if (name `elem` (map fst latexEmbeddedFormulaEnvs))
			  -- a formula environment -> we make a formlua XML element:
			then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
			     in  myConcatWithError (hasValue([content])) 
						   (makeContent restFrags TextAllowed parentEnv)
			else  -- No MMiSS-Env.
			  let beginDelimStr = case ps of
						(LParams _ _ (Just delimStr) _) -> delimStr
						otherwise -> ""
			      endDelimStr = case ps of
						(LParams _ _ _ (Just delimStr)) -> delimStr
						otherwise -> ""
			      begin = case name of
					"[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
					"{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
					otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
						      ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
			      end =  case name of 
				       "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
				       "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
				       otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
			      body = (makeContent fs TextAllowed parentEnv)
			      whole = myConcatWithError (myConcatWithError begin body) end
			  in myConcatWithError whole (makeContent frags TextAllowed parentEnv)
	(Command name ps) -> 
	   if (name `elem` (map fst includeCommands))
	     then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
		      delimElem = case ps of
				   (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
				   otherwise -> []
		  in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))]
						 ++ delimElem))
				       (makeContent frags TextAllowed parentEnv)
	     else if (name `elem` (map fst embeddedElements))
		    then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
			 in  myConcatWithError (hasValue([content])) 
					       (makeContent restFrags TextAllowed parentEnv)
		    else let delimStr = case ps of
				   (LParams _ _ (Just str) _) -> str
				   otherwise -> ""
			 in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
								      ++ (lparamsToString ps) ++ delimStr))]))
					       (makeContent frags TextAllowed parentEnv)
        (Special sType str) ->
          let piStr = (show sType) ++ " " ++ str
          in mapWithError ([(CMisc (PI (piSpecial, piStr)))] ++) (makeContent frags TextAllowed parentEnv)



{-- makeNamelessTextFragment:
String: Name des Vater-Environments
1. [Frag] : Eingangsliste: Liste der noch abzuarbeitenden Fragmente auf dieser Ebene
2. [Frag] : Textfragmente: Liste der Fragmente, die in dasselbe Textfragment eingehen
3. (Content, [Frag]): Das zusammengesetzt Textfragment-XML-Element sowie die restlichen Fragmente,
                      die nicht aufgenommen wurden.

Die Funktion bekommt eine Liste mit Fragmenten, von denen das erste ein Fragment sein sollte,
dass in ein Text-Element ohne Label eingepackt werden muss. Die Funktion geht die Liste
der Eingangsfragmente durch und sammelt in der Textfragmentliste alle nachfolgenden Fragmente
zusammen, die ebenfalls in das Text-Element übernommen werden können. Dies können sein:

- Other str  -> Strings
- Escaped Chars
- Embedded-Elemente (link, reference, define etc.)
- Formel-Environments

Findet die Funktion ein Fragment, dass nicht mehr in ein Textelement gehört, dann baut es 
mittels 'makeTextFragment' ein Element vom Typ 'text' zusammen und gibt dieses zusammen
mit der Liste der übriggebliebenen Fragmente zurück.
--}

makeNamelessTextFragment :: String -> [Frag] -> [Frag] -> (Content, [Frag])
makeNamelessTextFragment parentEnv [] textFrags = 
  ((makeTextFragment parentEnv "text" Nothing textFrags []), [])
makeNamelessTextFragment parentEnv (f:frags) textFrags = 
  case f of
    (Env name _ fs) -> 
       if (name `elem` ((map fst embeddedElements) ++ (map fst latexEmbeddedFormulaEnvs)))               
         then makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
         else 
           if (name `elem` (map fst mmiss2EnvIds))
             then let e1 = (makeTextFragment parentEnv "text" Nothing textFrags [])  
                      c1 = case e1 of
			      (CElem (Elem "paragraph" _ ((CElem(Elem _ _ c)):[]))) -> c
			      (CElem (Elem _ _ c)) -> c
	          in if ((length c1) > 1) 
                       then (e1, ([f] ++ frags))
                       else let c = head c1
		            in case c of
			         (CString _ str) -> 
				   if ((length (filter (not . (== '\n')) str) == 0) ||
			               ((head str) == '%')) 
				     then ((CMisc (Comment str)), ([f] ++ frags))
                                     else (e1, ([f] ++ frags))
                                 _ -> (e1, ([f] ++ frags))

--                             else makeNamelessTextFragment parentEnv (fs ++ frags) textFrags   -- Latex-Env.
             else makeNamelessTextFragment parentEnv frags (textFrags ++ [f])  -- Latex-Env.
    (Command "IncludeText" _) -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
    (Command name _) -> 
      if (name `elem` itemNames)
        then  ((makeTextFragment parentEnv "text" Nothing textFrags []), (f:frags))
        else if (name `elem` (map fst includeCommands))
               then let e1 = (makeTextFragment parentEnv "text" Nothing textFrags [])  
		        c1 = case e1 of
			       (CElem (Elem "paragraph" _ ((CElem(Elem _ _ c)):[]))) -> c
			       (CElem (Elem _ _ c)) -> c
	            in if ((length c1) > 1) 
                         then (e1, ([f] ++ frags))
		         else let c = head c1
			      in case c of
				  (CString _ str) -> 
				      if ((length (filter (not . (== '\n')) str) == 0) ||
			                 ((head str) == '%')) 
				        then ((CMisc (Comment str)), ([f] ++ frags))
                                        else (e1, ([f] ++ frags))
                                  _ -> (e1, ([f] ++ frags))
               else  makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
    _ -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])


makeTextFragment :: String -> String -> Maybe Params -> [Frag] -> [Content] -> Content

makeTextFragment parentEnv name params [] content = 
  let beginDelimElem = case params of
                         (Just (LParams _ _ (Just delimStr) _)) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                         otherwise -> []
      endDelimElem =   case params of
                         (Just (LParams _ _ _ (Just delimStr))) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                         otherwise -> []
      newContent = beginDelimElem ++ (concatTextElems(content)) ++ endDelimElem
  in if (parentEnv == "Section") 
       then (CElem (Elem "paragraph" [] 
                      [(CElem (Elem name (makeTextFragmentAttribs params) newContent))]))
       else (CElem (Elem name (makeTextFragmentAttribs params) newContent))

makeTextFragment parentEnv name params (f:frags) content =
  case f of
    (Other str) -> makeTextFragment parentEnv name params frags (content ++ [(CString True str)])
    (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                       in makeTextFragment parentEnv name params frags (content ++ [(CString True ("\\" ++ cstr))])
    (Command "Emphasis" ps) -> 
       let newElem = (CElem (Elem "emphasis" [] [(CString True (getEmphasisText ps))]))
           delimElem = case ps of
                          (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                          otherwise -> []
       in makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem) 
    (Command "IncludeText" ps) -> 
         let newElem = CElem (Elem "includeText" (makeIncludeAttribs ps) [])
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Link" ps) ->
         let newElem = CElem (Elem "link" (makeLinkAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Reference" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Ref" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Def" ps) ->
         let newElem = CElem (Elem "define" (makeDefineAttribs ps) (getDefineText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command cname ps) ->
         let delimStr = case ps of
                           (LParams _ _ (Just str) _) -> str
                           otherwise -> ""
             newElem = CMisc (PI (piInsertLaTeX, ("\\" ++ cname ++ (lparamsToString ps) ++ delimStr)))
 	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Special sType str) ->
          let piStr = (show sType) ++ " " ++ str
              newElem = [(CMisc (PI (piSpecial, piStr)))]
          in makeTextFragment parentEnv name params frags (content ++ newElem)
    (Env ename ps fs) -> 
      if (ename `elem` (map fst latexEmbeddedFormulaEnvs))
        then
              -- Fieser Trick: Um die Fragmente innerhalb der Formel-Umgebung in XML umzuwandeln,
              -- machen wir daraus einfach ein Textfragment mit eben dieser Funktion, in der wir uns gerade befinden
              -- und nehmen uns aus dem resultieren Element einfach den Content und stopfen ihn in das
              -- Formel-Element:
          let (CElem (Elem _ _ c)) = makeTextFragment "Text" name params fs []
              newElem = CElem (Elem "formula" (makeFormulaAttribs ename) c)
	  in  makeTextFragment parentEnv name params frags (content ++ [newElem])

        else
          let beginDelimStr = case ps of
                                (LParams _ _ (Just delimStr) _) -> delimStr
                                otherwise -> ""
              endDelimStr = case ps of
                               (LParams _ _ _ (Just delimStr)) -> delimStr
                               otherwise -> ""
              begin = case ename of
                        "[]" -> [CMisc (PI (piInsertLaTeX ,"[" ++ beginDelimStr))]
                        "{}" -> [CMisc (PI (piInsertLaTeX ,"{" ++ beginDelimStr))]
                        otherwise -> [CMisc (PI (piInsertLaTeX ,"\\begin{" ++ ename ++ "}" 
                                       ++ (lparamsToString ps) ++ beginDelimStr))]
              end = case ename of
                      "[]" -> [CMisc (PI (piInsertLaTeX ,"]" ++ endDelimStr))]
                      "{}" -> [CMisc (PI (piInsertLaTeX ,"}" ++ endDelimStr))]
                      otherwise -> [CMisc (PI (piInsertLaTeX ,"\\end{" ++ ename ++ "}" ++ endDelimStr))]
              (CElem (Elem _ _ c)) = makeTextFragment "Text" name params fs []
          in makeTextFragment parentEnv name params frags (content ++ begin ++ c ++ end)



makeListItem :: Params -> [Frag] -> [Content] -> (Content, [Frag])

makeListItem params [] contentList = 
  let delimElem = case params of
                     (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                     otherwise -> []
  in ((CElem (Elem "item" (makeListItemAttribs params) (delimElem ++ contentList))), [])

makeListItem params (f:frags) contentList =
   case f of
     (EscapedChar c) -> let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
                        in makeListItem params restFrags (contentList ++ [content])
     (Other str) -> 
        if (str /= "")
	  then let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
	  else makeListItem params frags contentList
     (Env name ps fs) -> 
        if (name `elem` (map fst plainTextAtoms))
          then
            let ename = if (name `elem` (map fst latexAtomFormulaEnvs))
                          then "formula"
                          else maybe "" snd (find ((name ==) . fst) plainTextAtoms)
                text = makeTextElem fs "" 
                attribs = (makeAttribs ps name)
            in  makeListItem params frags 
                             (contentList ++ [(CElem (Elem ename attribs
                                                          [CString True text]))])
          else
            if (name == "Text")
	      then makeListItem params frags 
                                (contentList ++ [(makeTextFragment "Item" "text" (Just(ps)) fs [])])
              else if (name `elem` (map fst listEnvs))
                     then makeListItem params frags 
                                       (contentList ++ coerceWithError(makeContent [f] TextAllowed "Item"))
                     else 
                       if (name `elem` (map fst latexEmbeddedFormulaEnvs))
                         -- als erstes Env. innerhalb eines ListItems kommt eine Formel-Umgebung -> Textfragment erzeugen
            	         then let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
                              in makeListItem params restFrags (contentList ++ [content]) 
                         else
                           if (not (name `elem` (map fst mmiss2EnvIds)))
                              -- Latex-Env. Inhalt auf diese Ebene ziehen:
                              then let beginFrag = case name of
                                                      "{}" -> [(Other "{")]
                                                      "[]" -> [(Other "[")]
		  				      otherwise -> [(Other ("\\begin{" ++ name ++ "}"))]	    
                                       endFrag = case name of
                                                    "{}" -> [(Other "}")]
                                                    "[]" -> [(Other "]")]
						    otherwise -> [(Other ("\\end{" ++ name ++ "}"))]	    
                                   in makeListItem params (beginFrag ++ fs ++ endFrag ++ frags) contentList  
		                 -- MMiSSLatex-Env. Ignorieren:
                              else makeListItem params frags contentList
     (Command "IncludeText" ps) -> 
        let newElem = CElem (Elem "includeText" (makeIncludeAttribs ps) [])
            delimElem = case ps of
                          (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                          otherwise -> []
        in makeListItem params frags (contentList ++ [newElem] ++ delimElem)
     (Command "IncludeAtom" ps) ->
        let newElem = (CElem (Elem "includeAtom" (makeIncludeAttribs ps) []))
            delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
        in makeListItem params frags (contentList ++ [newElem] ++ delimElem)

     (Command name ps) -> 
       if (name `elem` itemNames)
         then 
           let delimElem = case params of
                             (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                             _ -> []
           in ((CElem (Elem "item" (makeListItemAttribs params) (delimElem ++ contentList))), (f:frags))
         else 
          let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
          in makeListItem params restFrags (contentList ++ [content])

     (Special sType str) ->
          let piStr = (show sType) ++ " " ++ str
              newElem = [(CMisc (PI (piSpecial, piStr)))]
          in makeListItem params frags (contentList ++ newElem)

{--
     (Command name ps) -> 
        if (name `elem` (map fst embeddedElements))
          then let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
          else let newElem = CMisc (Comment ("\\" ++ name ++ (lparamsToString ps)))
               in makeListItem params frags (contentList ++ [newElem])
--}


concatTextElems :: [Content] -> [Content]

concatTextElems [] = []
concatTextElems ((CString True s1):((CString True s2):rest)) = concatTextElems ((CString True (s1 ++ s2)):rest)
concatTextElems ((CString True s1):((CElem e):rest)) = [(CString True s1), (CElem e)] ++ (concatTextElems rest)
concatTextElems ((CString True s1):((CMisc e):rest)) = [(CString True s1), (CMisc e)] ++ (concatTextElems rest)
concatTextElems ((CElem e):((CString True str):rest)) =
  [(CElem e)] ++ (concatTextElems ((CString True str):rest))
concatTextElems ((CElem e1):((CMisc e2):rest)) =
  [(CElem e1), (CMisc e2)] ++ (concatTextElems rest)
concatTextElems ((CMisc e1):e2:rest) = [(CMisc e1)] ++ (concatTextElems (e2:rest))
concatTextElems (e1:[]) = [e1]
concatTextElems (c:cs) = concatTextElems cs


-- detectTextMode ueberprueft anhand des uebergebenen Environment-Namens, ob darin laut MMiSS-Struktur
-- direkt Text enthalten sein darf. Es wird nicht ueberprueft, ob der Name ueberhaupt zu einem MMiSS-Env.
-- gehoert.
detectTextMode :: String -> Textmode
detectTextMode name = if (name `elem` (map fst (envsWithText ++ mmissPlainTextAtoms))) 
                        then TextAllowed
                        else NoText


makeAttribs :: Params -> String -> [Attribute]

makeAttribs ps name = 
  if (name `elem` (map fst includeCommands))
    then makeIncludeAttribs ps
    else if (name == "Text")
           then makeTextFragmentAttribs (Just(ps))
	   else if (name `elem` ["Link"])
	          then makeLinkAttribs ps
		  else if (name `elem` ["Reference", "Ref"]) 
		         then makeRefAttribs ps
			 else if (name == "Def")
			        then makeDefineAttribs ps
				else if (name `elem` (map fst (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs)))
                                       then let (LParams _ atts _ _) = id ps
				                normalAttrs = map convertAttrib atts
                                                formulaAttrs = makeFormulaAttribs name
                                            in normalAttrs ++ formulaAttrs
                                       else case ps of
				              (LParams _ atts _ _) -> map convertAttrib atts


makeIncludeAttribs :: Params -> [Attribute]
makeIncludeAttribs (LParams ((SingleParam ((Other labelId):[]) _):[]) atts _ _) =
  [("included", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeIncludeAttribs _ = []


makeTextFragmentAttribs :: Maybe(Params) -> [Attribute]
makeTextFragmentAttribs (Just (LParams _ atts _ _)) = map convertAttrib atts
makeTextFragmentAttribs _ = []

makeListItemAttribs :: Params -> [Attribute]
makeListItemAttribs (LParams _ atts _ _) = (map convertAttrib atts)


makeLinkAttribs :: Params -> [Attribute]
makeLinkAttribs (LParams ((SingleParam ((Other labelId):[]) _):ps) atts _ _) =
  [("linked", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)
makeLinkAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)


makeRefAttribs :: Params -> [Attribute]
makeRefAttribs  (LParams ((SingleParam ((Other labelId):[]) _):ps) atts _ _) =
  [("referenced", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)
makeRefAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)


makeDefineAttribs :: Params -> [Attribute]
makeDefineAttribs (LParams ((SingleParam ((Other labelId):[]) _):_) atts _ _) =
   [("defined", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "OptText")) . fst) atts)
makeDefineAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "OptText")) . fst) atts)


{-- makeFormulaAttribs weicht vom Schema der anderen makeXXXAttribs-Funktionen ab, da bei Formel-Umgebungen
    die nötigen Infos zum Befüllen der XML-Attribute nicht in den Parametern stecken, sondern im Namen
    des Environments, das als String übergeben wird:
--}
makeFormulaAttribs :: String -> [Attribute]
makeFormulaAttribs name =
  let latexEnv = maybe "" snd (find ((name ==) . fst) (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs))
  in  [("latexEnv", (AttValue [Left latexEnv]))]


-- getLinkText erwartet die Params eines Link oder Reference-Elementes und extrahiert daraus
-- den LinkText, der im ersten SingleParam steht und leer sein kann.

getLinkText :: Params -> [Content]
getLinkText (LParams _ atts _ _) =
  case (find ((== "LinkText") . fst) atts) of
    Just((_, linkText)) -> [(CString True linkText)]
    Nothing -> []

getDefineText :: Params -> [Content]
getDefineText (LParams _ atts _ _) =
  case (find ((== "OptText") . fst) atts) of
    Just((_, linkText)) -> [(CString True linkText)]
    Nothing -> []

getLabelFromParams :: Params -> String
getLabelFromParams (LParams _ atts _ _) =
  case (find ((== "Label") . fst) atts) of
    Just((_, label)) -> label
    Nothing -> ""


getEmphasisText :: Params -> String
getEmphasisText (LParams [] _ _ _) = ""
getEmphasisText (LParams ((SingleParam ((Other s):[]) _):ps) _ _ _) = s


convertAttrib :: (String, String) -> Attribute
convertAttrib (l, r) = ((attNameToXML l), AttValue [Left (latexToUnicode r)])


getAttribs :: [Attribute] -> String -> [String] -> String
getAttribs [] str _ = if ((take 1 str) == ",") 
                       then (drop 1 str)
                       else str  
getAttribs ((name, (AttValue [(Left value)])):as) str excludeList = 
   if (name `elem` (excludeList ++ glAttribsExclude))
     then getAttribs as str excludeList
     else if (value == "")
            then getAttribs as str excludeList
            else getAttribs as (str ++ "," ++ attNameToLatex(name) 
                                    ++ "={" ++ (unicodeToLatex value) ++ "}") excludeList 


{-- makeMMiSSLatex1 erzeugt aus einem XML-Element die zugehoerige MMiSSLatex-Repraesentation.
    Element ist das Root-Element des auszugebenden Dokumentbaumes, der Bool-Wert legt fest,
    ob das erzeugte LaTeX-Fragment ein komplettes File sein soll, dass ohne Änderungen
    geteXt werden kann (True), oder nicht (False). Wenn es komplett sein soll, dann wird 
    eine Praeambel erzeugt und 'includeXXX'-Elemente werden in MMiSSLaTeX-Include-Kommandos
    umgesetzt. Ist der Bool-Wert 'False', dann wird LaTeX für den XEmacs-Buffer erzeugt. In
    diesem Fall wird keine Preamble generiert und die Includes werden in spezielle EmacsContent-Objekte
    umgesetzt, die im MMiSS-XEmacs-Mode speziell behandelt werden.
--}


makeMMiSSLatex :: 
   (Element, Bool, [(MMiSSLatexPreamble,[MMiSSExtraPreambleData])]) 
   -> WithError (EmacsContent ((String, Char), [Attribute]))
   -- Each distinct preamble occurs once in the list, paired with a list
   -- for each of its call-sites.
makeMMiSSLatex (element,preOut,preambles') =
   -- stub function that doesn't use MMiSSExtraPreambleData for now.
   makeMMiSSLatex11 (element,preOut,map fst preambles')


makeMMiSSLatex11 :: (Element, Bool, [MMiSSLatexPreamble]) -> WithError (EmacsContent ((String, Char), [Attribute]))

makeMMiSSLatex11 ((Elem name atts content), preOut, preambles) = 
  let items = fillLatex preOut [(CElem (Elem name atts content))] []
      (p,_) = mergePreambles preambles
      preambleItem = [(EditableText (toString p))]
  in if preOut 
        then 
          let beginDocument = [EditableText "\\begin{document}\n"]
              endDocument = [EditableText "\n\\end{document}"]
           in hasValue((EmacsContent (preambleItem ++ beginDocument ++ items ++ endDocument)))
        else hasValue((EmacsContent items))


fillLatex :: Bool -> [Content] -> [EmacsDataItem ((String, Char), [Attribute])] 
               -> [EmacsDataItem ((String, Char), [Attribute])]

fillLatex out [] l = l

fillLatex out ((CElem (Elem "text" atts contents)):cs) inList = 
  let (b_insert, e_insert) = if (out && ((getParam "priority" atts) == "0"))
                               then ("\\begin{Included}\n", "\n\\end{Included}")
                               else ("","")
      s1 = "\\begin{Text}" 
      s2 = "[" ++ (getAttribs atts "" []) ++ "]"
      s3 = "\\end{Text}"
      items = if (s2 == "[]") 
                then (fillLatex out contents [])
                else [(EditableText (b_insert ++ s1 ++ s2))] 
                     ++ (fillLatex out contents []) 
                     ++ [(EditableText (s3 ++ e_insert))]
  in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem "item" atts contents)):cs) inList = 
   let s1 = "\\item" 
       attrStr = (getAttribs atts "" [])
       s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "] "
       items = [EditableText (s1 ++ s2)] ++ (fillLatex out contents [])
   in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem "emphasis" _ ((CString _ str):_))):cs) inList = 
   fillLatex out cs (inList ++ [EditableText ("\\Emphasis{" ++ str ++ "}")]) 

fillLatex out ((CElem (Elem ('i':'n':'c':'l':'u':'d':'e':unit) atts _)):cs) inList = 
   if (out == True) 
     then
       let label = "{" ++ (getParam "included" atts) ++ "}"
           name = (elemNameToLaTeX ("include" ++ unit))
           s1 = "{" ++ (getAttribs atts "" ["included"]) ++ "}"
           items = [(EditableText ("\\" ++ name ++ label ++ s1))]
       in fillLatex out cs (inList ++ items)
     else
       let labelId = getParam "included" atts
           item = [EmacsLink ((labelId, (fromIncludeStr unit)), atts)]
       in fillLatex out cs (inList ++ item)

-- Translates Link- and Reference-Embedded-Ops:
--
fillLatex out ((CElem (Elem name atts contents)):cs) inList
  | (name `elem` (map snd linkCommands)) =  
    let s1 = "\\" ++ (elemNameToLaTeX name) 
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                          in case c of
                              (CString _ body) -> "[" ++ body ++ "]"
                              _ -> ""
        s2 = "{" ++ (getParam "type" atts) ++ "}"
        s3 = "{" ++ (getParam "linked" atts) ++ "}"
        items = [(EditableText (s1 ++ phrase ++ s2 ++ s3))]
    in fillLatex out cs (inList ++ items)
  | (name `elem` (map snd refCommands)) =  
    let s1 = "\\" ++ (elemNameToLaTeX name) 
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                          in case c of
                              (CString _ body) -> "[" ++ body ++ "]"
                              _ -> ""
        s2 = "{" ++  (getParam "referenced" atts) ++ "}"
        items = [(EditableText (s1 ++ phrase ++ s2))]
    in fillLatex out cs (inList ++ items)
  | (name == "define") =
    let s1 = "\\" ++ (elemNameToLaTeX name)
        s2 = "{" ++ (getParam "defined" atts) ++ "}"
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                      in case c of
                           (CString _ body) -> "[" ++ body ++ "]"
                           _ -> ""
        items = [(EditableText (s1 ++ phrase ++ s2))]
    in fillLatex out cs (inList ++ items)
  | (name == "formula") =  
    let envType = getParam "latexEnv" atts
        latexEnv = maybe "" fst (find ((envType ==) . snd) (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs))
        (s1, s2) = case latexEnv of
                      "$" -> ("$", "$")
                      "$$" -> ("$$", "$$")
                      "\\(" -> ("\\(", "\\)")
                      "\\[" -> ("\\[", "\\]")
                      otherwise -> ("\\begin{" ++ latexEnv ++ "}", "\\end{" ++ latexEnv ++ "}")
    in fillLatex out cs (inList ++ [(EditableText s1)] ++ (fillLatex out contents []) ++ [(EditableText s2)])     
        
fillLatex out ((CString _ str):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (Comment str)):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (PI (pi, str))):cs) inList  
  | pi == piInsertLaTeX  =  fillLatex out cs (inList ++ [(EditableText str)])
  | pi == piSpecial =   if (out == True)
                          then let newStr = "%% Inserted by MMiSS repository:  " ++ str
                               in fillLatex out cs (inList ++ [(EditableText newStr)])
                          else fillLatex out cs inList
  | otherwise =  fillLatex out cs inList

fillLatex out ((CElem (Elem "package" atts contents)):cs) inList = 
  let s1 = "\\begin{Package}" 
      attrStr = if out 
                  then "Label={" ++ (getParam "label" atts) ++ "}"
                  else getAttribs atts "" []
      s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "]"
      s3 = "\\end{Package}"
      items = [(EditableText (s1 ++ s2))] ++ (fillLatex out contents []) 
              ++ [(EditableText s3)]
  in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem name atts contents)):cs) inList = 
  let (b_insert, e_insert) = if (out && ((getParam "priority" atts) == "0"))
                               then ("\\begin{Included}\n", "\n\\end{Included}")
                               else ("","")
      s1 = "\\begin{" ++ (elemNameToLaTeX name) ++ "}" 
      attrStr = getAttribs atts "" []
      s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "]"
      s3 = "\\end{" ++ (elemNameToLaTeX name) ++ "}"
      items = [(EditableText ( b_insert ++ s1 ++ s2))] ++ (fillLatex out contents []) 
              ++ [(EditableText (s3 ++ e_insert))]
  in fillLatex out cs (inList ++ items)

fillLatex out (c:cs) inList = fillLatex out cs inList



getParam :: String -> [Attribute] -> String
getParam name atts = let value = lookup name atts
                     in case value of
                          Just(AttValue [(Left str)]) -> str
                          Nothing -> ""


myConcatWithError :: WithError [a] -> WithError [a] -> WithError [a]

myConcatWithError l m = mapWithError (uncurry (++)) (pairWithError l m)


cElemListWithError:: String -> Params -> [Attribute] -> WithError [Content] -> WithError [Content]
cElemListWithError name ps atts c =
  case fromWithError c of
    Right content -> 
      let beginDelimElem = case ps of
                             (LParams _ _ (Just delimStr) _) -> [(CString True delimStr)]
                             otherwise -> []
          endDelimElem =   case ps of
                             (LParams _ _ _ (Just delimStr)) -> [(CString True delimStr)]
                             otherwise -> []
          newElemname = if (name == "list")
                          then if ((getParam "listType" atts) == "")
                                 then "itemize"
                                 else attNameToXML (getParam "listType" atts)
                          else name
          newAtts = filter (("listType" /=). fst ) atts
      in hasValue([(CElem (Elem newElemname newAtts (beginDelimElem ++ content)))] ++ endDelimElem)
    Left str -> hasError str


-- Maps an Xml tag to its corresponding mini-type if it has one.
-- (The mini-type is just something that identifies what sort of
-- include the String needs.)
classifyLabelledTag :: String -> Maybe Char
classifyLabelledTag str = fromIncludeStrOpt (mapLabelledTag str)

-- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
-- the corresponding includeXXX command.
toIncludeStr :: Char -> String
toIncludeStr 'G' = "Package"
toIncludeStr 'U' = "Unit"
toIncludeStr 'E' = "CompositeUnit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "Text"
toIncludeStr 'S' = "Section"
toIncludeStr 'c' = "ProgramComponent"
toIncludeStr 'm' = "Term"
toIncludeStr 'D' = "DevelopmentStep"
toIncludeStr 'P' = "Proof"
toIncludeStr 'o' = "ProofStep"
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"


-- | fromIncludeStrOpt
-- and also handles the case where the first letter is lower-cased.
fromIncludeStrOpt :: String -> Maybe Char
fromIncludeStrOpt "Package" = Just 'G'
fromIncludeStrOpt "Unit" = Just 'U'
fromIncludeStrOpt "Atom" = Just 'A'
fromIncludeStrOpt "Text" = Just 'T'
fromIncludeStrOpt "Section"          = Just 'S'                
fromIncludeStrOpt "CompositeUnit"    = Just 'E'        
fromIncludeStrOpt "ProgramComponent" = Just 'c'         
fromIncludeStrOpt "Proof"            = Just 'P'        
fromIncludeStrOpt "ProofStep"        = Just 'o'        
fromIncludeStrOpt "DevelopmentStep"  = Just 'D' 
fromIncludeStrOpt "Term"             = Just 'm' 
fromIncludeStrOpt (c : cs) | Char.isLower c 
   = fromIncludeStrOpt (toUpper c : cs)
fromIncludeStrOpt _ = Nothing


fromIncludeStr :: String -> Char
fromIncludeStr str = case fromIncludeStrOpt str of
   Just c -> c
   Nothing -> error 
    ("MMiSSDTDAssumptions.fromIncludeStr - bad include string"++str)

-- | Map tags to the name of their corresponding include element (minus
-- \"include\")
mapLabelledTag :: String -> String
mapLabelledTag s = 
   case s of
      "package" -> "Package"
      "paragraph" -> "Unit"
      "abstract" -> "Unit"
      "introduction" -> "Unit"
      "summary" -> "Unit"
      "theory" -> "Unit"
      "program" -> "Unit"
      "view" -> "Unit"
      "list" -> "CompositeUnit"
      "itemize" -> "CompositeUnit"
      "enumerate" -> "CompositeUnit"
      "description" -> "CompositeUnit"
      "example" -> "CompositeUnit"
      "exercise" -> "CompositeUnit"
      "assignment" -> "CompositeUnit"
      "solution" -> "CompositeUnit"
      "illustration" -> "CompositeUnit"
      "proposition" -> "CompositeUnit"
      "definition" -> "ConceptualUnit"
      "theorem" -> "CompositeUnit"
      "conjecture" -> "CompositeUnit"
      "falseConjecture" -> "CompositeUnit"
      "lemma" -> "CompositeUnit"
      "corollary" -> "CompositeUnit"
      "assertion" -> "CompositeUnit"
      "proof" -> "CompositeUnit"
      "development" -> "CompositeUnit"
      "comment" -> "Annotation"
      "note" -> "Annotation"
      "message" -> "Annotation"
      "error" -> "Annotation"
      "glossary" -> "Annotation"
      "bibEntry" -> "Atom"
      _ -> mapUpper s
   where
      mapUpper [] = []
      mapUpper (c : cs) = toUpper c : cs      



append :: a -> [a] -> [a]
append x xs = xs ++ [x]



--instance StringClass ImportCommands where
--   fromStringWE string = parseImportCommands string
--   toString importCmds = makeImportsText importCmds

-- --------------------------------------------------------------------------
-- PackageId is an instance of StringClass
-- --------------------------------------------------------------------------

instance StringClass PackageId where
   fromString = PackageId
   toString (PackageId str) = str

instance Show PackageId where
   showsPrec = qShow
