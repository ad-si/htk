module LaTeXParser (
   parseMMiSSLatex, -- :: String -> WithError Element
   -- Turn MMiSSLaTeX into an Element.   
   parseMMiSSLatexFile, -- :: SourceName -> IO (WithError Element)
   -- The same, for a file.
   makeMMiSSLatex -- (Element, Bool) -> WithError (EmacsContent String)
   -- Turns an Element into a MMiSSLaTeX source
   )
 where

-- module LaTeXParser where

import List
import Parsec
import Char
import XmlTypes
import Pretty hiding (char, spaces)
import qualified XmlPP as PP
import Computation hiding (try)
import ParsecError
import EmacsContent

type EnvId = String
type Command = String
type FormId = String
type LabelId = String
type Title = String
type Attributes = [(String, String)]
type Other = String

data SingleParam = SingleParam Frag Char    deriving Show

data Textmode = TextAllowed | NoText | TextFragment

{--------------------------------------------------------------------------------------------

 Ein LaTeX-Dokument besteht aus Fragmenten. Dies sind:
   Environments   Beginnen mit \begin{id} und enden mit \end{id}, oder unnamed: {..}
   Commands       Alles, was mit einem \ beginnt, ausser escaped Chars
   Escaped Chars  Sonderzeichen, bei denen die Befehlswirkung mit einem \ aufgehoben wurde
   Other          Textfragmente, die keiner der anderen Kategorien zugeordnet werden konnten

---------------------------------------------------------------------------------------------}

data Frag = Env  EnvId Params [Frag]               -- Environments e.g. \begin{document}..
          | Command Command Params                 -- \name params
          | EscapedChar Char                       -- Sonderzeichen: #$&~_^%{} 
          | Other Other deriving Show

data Params = MParams (Maybe FormId) LabelId Title Attributes   -- Parameter of MMiSS-Envs
            | LParams [SingleParam] (Maybe Attributes)          -- Parameter of LateX-Envs
              deriving Show

-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

{--
mmiss2EnvIds = ["Package","Section","Paragraph","View","Example","Exercise","Definition"] ++
               ["TextFragment","Table","Figure","GlossaryEntry", "Program","Theory","Theorem"] ++
               ["Conjecture","Lemma","Corollary","Assertion","Development","Proof","Script"] ++
               ["ProgramFragment","Clause","Step","Bibentry","Authorentry","List","Listitem"] ++
               ["Emphasis"]
--}
plainTextAtoms = [("Table","table"), ("Glossaryentry", "glossaryEntry"), ("Bibentry", "bibEntry")] ++
                 [("Figure", "figure"), ("ProgramFragment", "programFragment")] ++
                 [("Clause", "clause"), ("Step", "step"), ("Authorentry", "authorEntry")]

envsWithText = [("Section", "section"), ("Paragraph", "paragraph"), ("Abstract", "abstract")] ++
               [("Introduction", "introduction"),  ("Summary", "summary"), ("Program","program")] ++
	       [("Exercise","exercise"), ("Example","example"),  ("Definition","definition")] ++
               [("Theory","theory"), ("Theorem", "theorem"), ("Development","development")] ++
               [("Proof","proof"), ("Script","script"), ("List", "list"), ("ListItem", "listItem")] ++
               [("Conjecture", "conjecture"), ("Lemma", "lemma"), ("Corollary", "corollary")] ++
               [("Assertion", "assertion")]

envsWithoutText = [("Package", "package")]

includeCommands =  [("IncludeGroup", "includeGroup"), ("IncludeUnit", "includeUnit")] ++
                   [("IncludeAtom", "includeAtom"), ("IncludeTextFragment","includeTextFragment")]

linkAndRefCommands = [("Link", "link"), ("ForwardLink","link"), ("Reference","reference")] ++
                     [("ForwardReference", "reference")]

embeddedElements = [("Emphasis","emphasis"), ("IncludeTextFragment","includeTextFragment")] ++
		   [("Link","link") , ("Define", "define"), ("Reference", "reference")] ++
                   [("ForwardLink","link"), ("ForwardReference", "reference")]

mmiss2EnvIds = plainTextAtoms ++ envsWithText ++ envsWithoutText

---------------------------------------------------------------------------------------------
--
-- Hier beginnen die Parser
--
---------------------------------------------------------------------------------------------

backslash :: GenParser Char st Char
backslash = char '\\'

idParser :: GenParser Char st String
idParser = many (noneOf "}]")

-- commaSep :: GenParser Char st a
commaSep p = p `sepBy` (oneOf ",")

-- equalSep p = p `sepBy` (oneOf "=")

listTypeParser :: GenParser Char st String
listTypeParser = try(string "itemize") <|> try(string "enumeration") <|> return ("")


{- attParser parses the list of attributes belonging to an MMiSS-Environment -}

attParser :: GenParser Char st Attributes
attParser = commaSep attribute
            <|> return([])

attribute :: GenParser Char st (String, String)
attribute = do spaces
               key <- try(many1(noneOf ",=}")) <?> "attribute name"
               spaces
               try(char '=') <?> "value for attribute '" ++ key ++ "'"
               spaces
               v <- try(choice ((delimitedValue key):(value:[]))) 
                    <?> "value for attribute '" ++ key ++ "'"
               return (key, v)

delimitedValue :: String -> GenParser Char st String
delimitedValue key = between (char '{') (char '}') (value1 key)

value :: GenParser Char st String
value = many1 (noneOf ",}")

value1 :: String -> GenParser Char st String
value1 key = try(many1 (noneOf "}")) <?> "value for attribute '" ++ key ++ "'" 


{- genParam parses an arbitrary Parameter of a LaTeX-Environment or Command (no Parameter of
   MMiSSLaTeX-Environments - these are handled by "attParser".
   Because LaTeX-Environment-Parameters can be delimited by either '{' or '[' or '('
   (the latter since LaTeX 2e I think), the l und r parameters to genParam
   hold the delimiter character, to look out for. e.g. l = '{', r = '}'  -}

genParam :: Char -> Char -> GenParser Char st Frag
genParam l r = between (char l) (char r) (otherDelim r) <?> ("fragment between " ++ [l] ++ " and " ++ [r])


-- other konsumiert solange Text, bis ein Backslash oder Kommentarzeichen auftaucht und
-- generiert ein Other-Fragment mit diesem Text.

other :: GenParser Char st Frag
other = fmap Other (many1 (noneOf "\\%"))


-- otherDelim konsumiert solange Text, bis ein Backslash, Kommentarzeichen oder ein
-- Delimiter (r) auftaucht. Dient dazu, um Other-Fragmente in Parametern zu erkennen;
-- in Parametern darf das schliessende Zeichen nicht ueberlesen werden.

otherDelim :: Char -> GenParser Char st Frag
otherDelim r = let s = "\\%" ++ [r]
               in fmap Other (many1 (noneOf s))


-- begin erkennt den Namen einer Umgebung (id)

begin :: GenParser Char st String
begin = do  try (string "begin")
            spaces
            c <- between (char '{') (char '}') idParser
            spaces
            return(c)
                            
-- end  ueberprueft, ob als naechstes ein \end{id} in der Source auftaucht.

end :: String -> GenParser Char st String
end id = do backslash
            string "end"
            spaces 
            c <- between (char '{') (char '}') idParser
            return(c)


-- continue wird vom Parser 'beginBlock' benutzt, um nach dem schliessenden Tag fuer
-- 'id' zu suchen. Der Inhalt der vom beginBlock erkannten Umgebung wird durch den
-- frag-Parser geschickt.

continue ::  [Frag] -> String -> GenParser Char st [Frag]
continue l id = (try (end id) >>= 
                       (\ x -> if x == id then return l else fail ("no matching end-Tag for <" ++ id ++ ">")))
                <|> do f <- frag
                       continue (f:l) id

-- beginBlock erkennt den Start einer Umgebung (\begin{id}) und parst mittels
-- 'continue' den Inhalt der Umgebung. 'continue' achtet darauf, dass es zu der 
-- id aus dem begin-Block auch ein \end{id}-Block gibt.  
               
beginBlock :: GenParser Char st Frag
beginBlock = do id <- begin
                params <- envParams id
                l <- continue [] id
                return (Env id params (reverse l))


-- envParams unterscheidet MMiSSLatex-Umgebungen von Latex-Umgebungen und stoesst
-- die passende Erkennungsfunktion fuer die Parameter an.

envParams :: String -> GenParser Char st Params
envParams id =  if (id `elem` (map fst mmiss2EnvIds)) 
		    then mEnvParams id 
                    else lParams id [] <?> ("Parameters for LaTeX-Environment <" ++ id ++ ">")


-- mListParams erkennt die Parameter, die zu einer MMiSSLatex-Umgebung gehoeren 
-- (mit Ausnahme von List-Umgebungen. Dies sind: 
-- [FormalismID] {LabelID} {Title} {Attributes}

mEnvParams :: String -> GenParser Char st Params
mEnvParams "List" = do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
		       spaces
		       labelId <-  try(between (char '{') (char '}') idParser)
			           <?> "{labelID}{title}{attribute-list} for Environment <List>"   
		       spaces
                       listType <- try(between (char '{') (char '}') listTypeParser)
                                   <?> "listtype 'itemize' or 'enumeration' for Environment <list>"
	               spaces
		       attributes <- try(between (char '{') (char '}') attParser)
				     <?> "{attribute-list} for Environment <List>"   
		       if formId == "" 
		         then return(MParams Nothing labelId "" (("type",listType):attributes))
		         else return(MParams (Just(formId)) labelId "" (("type",listType):attributes))

mEnvParams "ListItem" = 
  do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
     spaces
     labelId <-  try(between (char '{') (char '}') idParser)
	         <?> "{labelID}{attribute-list} for Environment <ListItem>"   
     spaces
     attributes <- try(between (char '{') (char '}') attParser)
     	           <?> "{attribute-list} for Environment <ListItem>"   
     if formId == "" 
       then return(MParams Nothing labelId "" attributes)
       else return(MParams (Just(formId)) labelId "" attributes)

mEnvParams id = do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
		   spaces
		   labelId <-  try(between (char '{') (char '}') idParser)
			       <?> "{labelID}{title}{attribute-list} for Environment <" ++ id ++ ">"   
		   spaces
		   
		   title <- try(between (char '{') (char '}') idParser)
			    <?> "{title}{attribute-list} for Environment <" ++ id ++ ">"   
		   spaces
		   attributes <- try(between (char '{') (char '}') attParser)
				 <?> "{attribute-list} for Environment <" ++ id ++ ">"   
		   if formId == "" 
		     then return(MParams Nothing labelId title attributes)
		     else return(MParams (Just(formId)) labelId title attributes)


-- lParams erkennt Parameter eines Latex-Commands. Diese koennen in normalen, geschweiften
-- oder eckigen Klammern eingeschlossen sein. Es wird nicht geprueft, ob die Parameter
-- und ihre Reihenfolge den Latex-Definitionen entsprechen.
	    
lParams :: String -> [SingleParam] -> GenParser Char st Params
lParams id l
  | id == "Emphasis" = do spaces
                          p <- try ( genParam '{' '}' )
                          return (LParams [(SingleParam p '{')] Nothing)
  | id `elem` (map fst includeCommands) =
      do spaces
	 labelId <-  try(between (char '{') (char '}') idParser)
	             <?> "{referencedLabelID}{attribute-list} for Command <" ++ id ++ ">"   
	 spaces
	 attributes <- try(between (char '{') (char '}') attParser)
		       <?> "{attribute-list} for Environment <" ++ id ++ ">"   
         if (attributes == []) 
           then return (LParams [(SingleParam (Other labelId) '{')] Nothing)
           else return (LParams [(SingleParam (Other labelId) '{')] (Just (attributes)))
  | id `elem` (map fst linkAndRefCommands) =
      do optFrag <-  option (Other "") (try ( genParam '[' ']' ))
         spaces
	 labelId <-  try(between (char '{') (char '}') idParser)
	             <?> "[text]{referencedLabelID}{attribute-list} for Command <" ++ id ++ ">"   
	 spaces
	 attributes <- try(between (char '{') (char '}') attParser)
		       <?> "{attribute-list} for Environment <" ++ id ++ ">"   
         if (attributes == []) 
           then return (LParams [(SingleParam optFrag '['), (SingleParam (Other labelId) '{')] Nothing)
           else return (LParams [(SingleParam optFrag '['), (SingleParam (Other labelId) '{')] (Just(attributes)))
 
 | id == "Define" = 
      do labelId <-  try(between (char '{') (char '}') idParser)
	             <?> "{labelID}{definedName}{attribute-list} for Command <" ++ id ++ ">"   
         spaces
	 definedName <-  (try ( genParam '{' '}' ))
	                 <?> "{labelID}{definedName}{attribute-list} for Command <" ++ id ++ ">"
   	 spaces
	 attributes <- try(between (char '{') (char '}') attParser)
		       <?> "{attribute-list} for Environment <" ++ id ++ ">"   
         if (attributes == []) 
           then return (LParams [(SingleParam (Other labelId) '{'), (SingleParam definedName '{')] Nothing)
           else return (LParams [(SingleParam (Other labelId) '{'), (SingleParam definedName '{')] (Just(attributes)))
 | id == "ListItem" = 
      do attributes <- try(between (char '{') (char '}') attParser)
		       <?> "{attribute-list} for Environment <" ++ id ++ ">"   
         if (attributes == []) 
           then return (LParams [] Nothing)
           else return (LParams [] (Just(attributes)))
 | otherwise = do p <- try ( genParam '{' '}' )
                  lParams id ((SingleParam p '{'):l)  
               <|>  do p <- try ( genParam '[' ']' )
                       lParams id ((SingleParam p '['):l)
               <|>  do p <- try ( genParam '(' ')' )
                       lParams id ((SingleParam p '('):l)
               <|>  return (LParams (reverse l) Nothing)

{--
continueAdhocEnv l = do try (char '}')
                        return l
                     <|> do f <- frag
                            continueAdhocEnv (f:l) 
                         

-- adhocEnvironment erkennt Umgebungen ohne Namen: ...{text}...

adhocEnvironment = do char '{'
                      l <- continueAdhocEnv [] <?> "closing } for unnamed environment"
                      return (Env "" (LParams []) (reverse l))
--}


-- command erkennt LaTeX-Kommandos. Escaped letters wie \{, \$ etc. werden hier 
-- nicht erkannt, sondern von escapedChar geparst.

command :: GenParser Char st Frag
command = do c <- many1 (noneOf "\\\v\f\t\r\n{[(}]} \"")
             skipMany (oneOf " \t")
             l <- lParams c [] 
	     return (Command c l)

-- escapedChar erkennt die mit Backslash ihrer Sonderbedeutung in LaTeX entbundenen
-- Zeichen sowie das geschuetzte Leerzeichen.

escapedChar :: GenParser Char st Frag
escapedChar = do c <- try (oneOf "\\#$&~_^%{} ")
                 return (EscapedChar c)


-- comment erkennt Kommentarzeilen. Kommentare werden als Other-Fragment behandelt
--

comment :: GenParser Char st Frag
comment = do char '%'
             s <- manyTill anyChar (try newline)
             return (Other ("%" ++ s ++ "\n"))


-- frag erkennt Latex-Fragmente: Kommentare, Environments, Commands und Escaped Chars. 
-- Alle anderen Zeichenfolgen werden in das Fragment 'other' verpackt.

frag :: GenParser Char st Frag
frag = comment
	 <|> do backslash
		beginBlock <|> escapedChar <|> command <|> return (Other "\\")
  --     <|> adhocEnvironment
	 <|> other
		

-- latexDoc ist der Haupt-Parser. Er sammelt die Fragmente der Root-Ebene ein und kapselt
-- sie in eine virtuelle Root-Umgebung, damit eine saubere Baum-Struktur entsteht. 

latexDoc :: [Frag] -> GenParser Char st Frag
latexDoc l =  do f <-  frag <?> "Fragment"
		 latexDoc (f:l)
	      <|> return (Env "Root" (LParams [] Nothing) (reverse l))


-- Haupt-Funktion (eigentlich main)

mparse :: SourceName -> IO ()
mparse fname = do result <- parseFromFile (latexDoc []) fname
                  case result of 
                      Left err -> print err
                      Right f -> print f



{-- Main function: Parses the given MMiSSLatex-string and returns an Element which holds the
    XML-structure.  --}

parseMMiSSLatex :: String -> WithError Element

parseMMiSSLatex s = let result = parse (latexDoc []) "" s
		    in case result of
			 Right ast  -> makeXML ast
			 Left err -> hasError (concat (map messageString (errorMessages(err))))


parseMMiSSLatexFile :: SourceName -> IO (WithError Element)

parseMMiSSLatexFile s = do result <- parseFromFile (latexDoc []) s
 		           case result of
			     Right ast  -> return(makeXML ast)
			     Left err -> return(hasError (concat (map messageString (errorMessages(err)))))

parseAndShow :: SourceName -> IO ()

parseAndShow s = do result <- parseFromFile (latexDoc []) s
 		    case result of
			Right ast  -> print ast
			Left err -> print err


showElement :: WithError Element -> String
showElement e = coerceWithError (mapWithError (render . PP.element) e)



makeXML :: Frag -> WithError Element
makeXML frag = let rootElem = coerceWithError(findFirstPackage [frag] [] False)
	       in  hasValue(rootElem)


findFirstPackage :: [Frag] -> [Frag] -> Bool -> WithError Element

findFirstPackage ((Env "Root" _ fs):[]) _ _  = findFirstPackage fs [] True
findFirstPackage ((Env "document" _ fs):_) preambleFs _ = findFirstPackage fs preambleFs False
findFirstPackage ((Env "Package" ps fs):_) preambleFs _ = 
  let atts = makeAttribs ps
      content = coerceWithError (makeContent fs NoText "package")
      preambleComment = (CMisc (Comment (makeTextElem preambleFs)))
  in hasValue(Elem "package" atts ([preambleComment] ++ content))
findFirstPackage (f:fs) preambleFs True = findFirstPackage fs (preambleFs ++ [f]) True
findFirstPackage [] _ _  = hasError("No topmost 'package' element found!")           


makeContent :: [Frag] -> Textmode -> String -> WithError [Content]

makeContent [] _ _ = hasValue([])
makeContent (f:frags) NoText parentEnv = 
   case f of
--  TODO: (EscapedChar c) -> Merken, da er sonst verschwindet (und das kann ein \\-Zeilenumbruch sein)
     (Other str) -> if ((length (filter (not . (== '\n')) str) == 0) ||
			((head str) == '%'))
                      then hasValue([(CMisc (Comment str))] ++ coerceWithError(makeContent frags NoText parentEnv))
		      else hasError("No text allowed inside a " ++ parentEnv ++ "!")
		      -- TODO: Text, der nur aus Linefeeds besteht, muss erhalten bleiben, da er Einfluss
                      --       auf das von Latex erzeugte Layout haben kann.
     (Env name ps fs) -> 
       if (name `elem` (map fst plainTextAtoms))
         then hasError("No Environment '" ++ name ++ "' allowed inside a " ++ parentEnv ++ "!")
         else
           if (name == "TextFragment")
	     then hasError("No Environment 'TextFragment' allowed inside a " ++ parentEnv ++ "!")
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in hasValue([(CElem (Elem ename (makeAttribs ps)
	                                        (coerceWithError(makeContent fs (detectTextMode name) name))))] 
                                  ++ coerceWithError(makeContent frags NoText parentEnv))
                 else  -- No MMiSS-Env.
		   hasValue(coerceWithError(makeContent fs NoText parentEnv) 
                            ++ coerceWithError(makeContent frags NoText parentEnv))
     (Command name ps) -> if (name `elem` (map fst includeCommands))      -- TODO: Referencen anlegen
			    then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
				 in hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))]
                                             ++ coerceWithError(makeContent frags NoText parentEnv))
			    else makeContent frags NoText parentEnv
     _ -> makeContent frags NoText parentEnv

makeContent (f:frags) TextAllowed "List" = 
   case f of
     (Env name ps fs) -> 
               if (name `elem` (map fst mmiss2EnvIds))
	         then hasError("Environment '" ++ name ++ "' is not allowed in lists. Wrap it up with a ListItem.")
                 else  -- No MMiSS-Env.
		   hasValue(coerceWithError(makeContent fs TextAllowed "List") 
                            ++ coerceWithError(makeContent frags TextAllowed "List"))

     (Command "ListItem" ps) -> 
         let (content, restFrags) = makeListItem ps frags []
         in hasValue([content] ++ coerceWithError(makeContent restFrags TextAllowed "List"))
     _ -> makeContent frags TextAllowed "List"

makeContent (f:frags) TextAllowed parentEnv = 
   case f of
     (EscapedChar c) -> 
       let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
       in hasValue([content] ++ coerceWithError(makeContent restFrags TextAllowed parentEnv))
     (Other str) -> 
       if (str /= "")
	 then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
              in hasValue([content] ++ coerceWithError(makeContent restFrags TextAllowed parentEnv))
	 else makeContent frags TextAllowed parentEnv
     (Env name ps fs) -> 
       if (name `elem` (map fst plainTextAtoms))
         then
           let ename = maybe "" snd (find ((name ==) . fst) plainTextAtoms)
           in  hasValue([(CElem (Elem ename (makeAttribs ps) [CString True (makeTextElem fs)]))] 
                        ++ coerceWithError(makeContent frags TextAllowed parentEnv))
         else
           if (name == "TextFragment")
	     then hasValue([(makeTextFragment parentEnv name (Just(ps)) fs [])] 
                           ++ coerceWithError(makeContent frags TextAllowed parentEnv))
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in hasValue([(CElem (Elem ename (makeAttribs ps)
	                                        (coerceWithError(makeContent fs (detectTextMode name) name))))] 
                               ++ coerceWithError(makeContent frags TextAllowed parentEnv))
                 else  -- No MMiSS-Env.
		   hasValue(coerceWithError(makeContent fs TextAllowed parentEnv) 
                            ++ coerceWithError(makeContent frags TextAllowed parentEnv))
     (Command name ps) -> if (name `elem` (map fst includeCommands))
			    then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
				 in hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))]
                                             ++ coerceWithError(makeContent frags TextAllowed parentEnv))
			    else makeContent frags TextAllowed parentEnv
     _ -> makeContent frags TextAllowed parentEnv


makeTextElem :: [Frag] -> String 

makeTextElem [] = ""
makeTextElem (f:fs) = 
  case f of
    (EscapedChar c) -> "\\" ++ [c] ++ (makeTextElem fs)
    (Other str) -> str ++ (makeTextElem fs)
    (Command name ps) -> "\\" ++ name ++ (lparamsToString ps) ++ (makeTextElem fs)
    (Env name ps content) -> "\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++
				(makeTextElem content) ++
			     "\\end{" ++ name ++ "}" ++ (makeTextElem fs) 
    _ -> makeTextElem fs


lparamsToString :: Params -> String
lparamsToString (LParams singleParams (Just(atts))) = 
  (concat (map singleParamToString singleParams)) ++ (getAttribs (map convertAttrib atts) "" [])
lparamsToString (LParams singleParams Nothing) =  (concat (map singleParamToString singleParams))
lparamsToString _ = ""


singleParamToString :: SingleParam -> String
singleParamToString (SingleParam f '{') = "{" ++ (makeTextElem [f]) ++ "}"
singleParamToString (SingleParam f '[') = "[" ++ (makeTextElem [f]) ++ "]" 
singleParamToString (SingleParam f '(') = "(" ++ (makeTextElem [f]) ++ ")"


makeNamelessTextFragment :: String -> [Frag] -> [Frag] -> (Content, [Frag])
makeNamelessTextFragment parentEnv [] textFrags = 
  ((makeTextFragment parentEnv "textFragment" Nothing textFrags []), [])
makeNamelessTextFragment parentEnv (f:frags) textFrags = 
  case f of
    (Env name _ fs) -> if (name `elem` (map fst embeddedElements))              
		         then makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
		         else 
                           if (name `elem` (map fst mmiss2EnvIds))
                             then let e1 = (makeTextFragment parentEnv "textFragment" Nothing textFrags [])  
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

                             else makeNamelessTextFragment parentEnv (fs ++ frags) textFrags   -- Latex-Env.
    (Command "ListItem" _) -> ((makeTextFragment parentEnv "textFragment" Nothing textFrags []), (f:frags))
    _ -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])


makeTextFragment :: String -> String -> Maybe Params -> [Frag] -> [Content] -> Content

makeTextFragment parentEnv name params [] content = 
  if (parentEnv == "Section") 
    then (CElem (Elem "paragraph" [] 
                      [(CElem (Elem name (makeTextFragmentAttribs params) (concatTextElems(content))))]))
    else (CElem (Elem name (makeTextFragmentAttribs params) (concatTextElems(content))))

makeTextFragment parentEnv name params (f:frags) content =
  case f of
    (Other str) -> makeTextFragment parentEnv name params frags (content ++ [(CString True str)])
    (EscapedChar c) -> makeTextFragment parentEnv name params frags (content ++ [(CString True ("\\" ++ [c]))])
    (Command "Emphasis" ps) -> let newElem = (CElem (Elem "emphasis" [] [(CString True (getEmphasisText ps))]))
			        in makeTextFragment parentEnv name params frags (content ++ [newElem]) 
    (Command "IncludeTextFragment" ps) -> 
         let newElem = CElem (Elem "includeTextFragment" (makeIncludeAttribs ps) [])
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Link" ps) ->
         let newElem = CElem (Elem "link" (makeLinkAttribs ps "present") (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "ForwardLink" ps) -> 
         let newElem = CElem (Elem "link" (makeLinkAttribs ps "absent") (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Reference" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps "present") (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "ForwardReference" ps) -> 
         let newElem = CElem (Elem "reference" (makeRefAttribs ps "absent") (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Define" ps) ->
         let newElem = CElem (Elem "define" (makeDefineAttribs ps) (getDefineText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Env _ _ fs) -> makeTextFragment parentEnv name params (fs ++ frags) content
    _ -> makeTextFragment parentEnv name params frags content                         



makeListItem :: Params -> [Frag] -> [Content] -> (Content, [Frag])

makeListItem params [] contentList = 
   ((CElem (Elem "listItem" (makeAttribs params) contentList)), [])

makeListItem params (f:frags) contentList =
   case f of
     (Other str) -> 
        if (str /= "")
	  then let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
	  else makeListItem params frags contentList
     (Env name ps fs) -> 
        if (name `elem` (map fst plainTextAtoms))
          then
            let ename = maybe "" snd (find ((name ==) . fst) plainTextAtoms)
            in  makeListItem params frags 
                             (contentList ++ [(CElem (Elem ename (makeAttribs ps)
                                                          [CString True (makeTextElem fs)]))])
          else
            if (name == "TextFragment")
	      then makeListItem params frags 
                                (contentList ++ [(makeTextFragment "ListItem" "textFragment" (Just(ps)) fs [])])
              else if (name == "List")
                     then makeListItem params frags 
                                       (contentList ++ coerceWithError(makeContent [f] TextAllowed "ListItem"))
                     else if (not (name `elem` (map fst mmiss2EnvIds)))
                                 -- Latex-Env. Inhalt auf diese Ebene ziehen:
                            then makeListItem params (fs ++ frags) contentList  
		                 -- MMiSSLatex-Env. Ignorieren:
                            else makeListItem params frags contentList
     (Command "IncludeTextFragment" ps) -> 
       let newElem = CElem (Elem "includeTextFragment" (makeIncludeAttribs ps) [])
       in makeListItem params frags (contentList ++ [newElem])
     (Command "IncludeAtom" ps) ->
       let newElem = (CElem (Elem "includeAtom" (makeIncludeAttribs ps) []))
       in makeListItem params frags (contentList ++ [newElem])
     (Command "ListItem" ps) -> ((CElem (Elem "listItem" (makeAttribs params) contentList)), (f:frags))
     _ -> makeListItem params frags contentList


concatTextElems :: [Content] -> [Content]

concatTextElems [] = []
concatTextElems ((CString True s1):((CString True s2):rest)) = concatTextElems ((CString True (s1 ++ s2)):rest)
concatTextElems ((CString True s1):((CElem e):rest)) = [(CString True s1), (CElem e)] ++ (concatTextElems rest)
concatTextElems ((CElem e):((CString True str):rest)) =
  [(CElem e)] ++ (concatTextElems ((CString True str):rest))
concatTextElems (e1:[]) = [e1]

{--
addText :: [Content] -> String -> [Content]
addText [] str =  [(CString True str)]
addText (xs:x:[]) = 
  case x of
    (CString b cstr) -> xs ++ [(CString b cstr ++ str)]
    e -> xs ++ [e] ++ [(CString True str)]
--}


-- detectTextMode ueberprueft anhand des uebergebenen Environment-Namens, ob darin laut MMiSS-Struktur
-- direkt Text enthalten sein darf. Es wird nicht ueberprueft, ob der Name ueberhaupt zu einem MMiSS-Env.
-- gehoert.
detectTextMode :: String -> Textmode
detectTextMode name = if (name `elem` (map fst envsWithText)) then TextAllowed
                       else NoText



--  MParams (Maybe FormId) LabelId Title Attributes
makeAttribs :: Params -> [Attribute]
makeAttribs 
  (MParams Nothing label title atts) = let p1 = if (label == "") then []
						  else [("label", (AttValue [Left label]))]
                                           p2 = if (title == "") then []
						  else [("title", (AttValue [Left title]))]
				       in p1 ++ p2 ++ (map convertAttrib atts)
makeAttribs 
  (MParams (Just formID) label title atts) =  let p1 = [("notationID", (AttValue [Left formID]))]
					          p2 = if (label == "") then []
						         else [("label", (AttValue [Left label]))]
                                                  p3 = if (title == "") then []
						         else [("title", (AttValue [Left title]))]
				              in p1 ++ p2 ++ p3 ++ (map convertAttrib atts)
makeAttribs _ = []
  

makeIncludeAttribs :: Params -> [Attribute]
makeIncludeAttribs (LParams ((SingleParam (Other labelId) _):[]) (Just(atts))) =
  [("included", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeIncludeAttribs (LParams ((SingleParam (Other labelId) _):[]) Nothing) =
  [("included", (AttValue [Left labelId]))]
makeIncludeAttribs _ = []


makeTextFragmentAttribs :: Maybe(Params) -> [Attribute]
makeTextFragmentAttribs Nothing = []
makeTextFragmentAttribs (Just((LParams ((SingleParam (Other labelId) _):[]) (Just(atts))))) =
  [("label", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeTextFragmentAttribs (Just((LParams ((SingleParam (Other labelId) _):[]) Nothing))) =
  [("label", (AttValue [Left labelId]))]
makeTextFragmentAttribs _ = []


makeLinkAttribs :: Params -> String -> [Attribute]
makeLinkAttribs (LParams ps (Just(atts))) status =
  [("linked", (AttValue [Left labelId])), ("status", (AttValue [Left status]))] ++ (map convertAttrib atts)
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeLinkAttribs (LParams ps Nothing) status =
  [("linked", (AttValue [Left labelId])), ("status", (AttValue [Left status]))]
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeLinkAttribs _ _ = []


makeRefAttribs :: Params -> String -> [Attribute]
makeRefAttribs (LParams ps (Just(atts))) status =
  [("referenced", (AttValue [Left labelId])), ("status", (AttValue [Left status]))] ++ (map convertAttrib atts)
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeRefAttribs (LParams ps Nothing) status =
  [("referenced", (AttValue [Left labelId])), ("status", (AttValue [Left status]))]
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeRefAttribs _ _ = []


makeDefineAttribs :: Params -> [Attribute]
makeDefineAttribs (LParams ((SingleParam (Other labelId) _):_) (Just(atts))) =
  [("label", (AttValue [Left labelId]))] ++ (map convertAttrib atts)


getLinkText :: Params -> [Content]
getLinkText (LParams ((SingleParam (Other str) _):_) _) = 
  if (str == "") 
    then []
    else [(CString True str)]


getDefineText :: Params -> [Content]
getDefineText  (LParams ps _ ) =
  if (text == "") 
    then []
    else [(CString True text)]
  where
    (SingleParam (Other text) _) = genericIndex ps 1


convertAttrib :: (String, String) -> Attribute
convertAttrib (l, r) = (l, AttValue [Left r])


getEmphasisText :: Params -> String
getEmphasisText (LParams [] _) = ""
getEmphasisText (LParams ((SingleParam (Other s) _):ps) _) = s



{-- makeMMiSSLatex erzeugt aus einem XML-Element die zugehoerige MMiSSLatex-Repraesentation.
--}

makeMMiSSLatex :: (Element, Bool) -> WithError (EmacsContent String)
makeMMiSSLatex ((Elem "textFragment" atts contents), False) = 
   let s1 = "\\begin{TextFragment}" 
       s2 = "[" ++ (getParam "notationID" atts) ++ "]"
       s3 = "{" ++ (getParam "label" atts) ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["notationID", "label"]) ++ "}\n"
       s5 = "\\end{TextFragment}\n"
       str = s1 ++ s2 ++ s3 ++ s4 ++ (fillLatex contents "") ++ s5
   in hasValue(EmacsContent [EditableText str])

makeMMiSSLatex ((Elem "list" atts contents), False) = 
   let s1 = "\\begin{List}" 
       s2 = "{" ++ (getParam "label" atts) ++ "}"
       listtype = getParam "type" atts
       s3 = "{" ++ listtype ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["type", "label"]) ++ "}\n"
       s5 = "\\end{List}\n"
       str = s1 ++ s2 ++ s3 ++ s4 ++ (fillLatex contents "") ++ s5
   in if (listtype == "") 
        then hasError("Missing listtype attribute for Element " ++ s2 ++ " !")
	else hasValue(EmacsContent [EditableText str])

makeMMiSSLatex ((Elem name atts contents), False) = 
   let s1 = "\\begin{" ++ (toLatexName name) ++ "}" 
       s2 = "[" ++ (getParam "notationID" atts) ++ "]"
       s3 = "{" ++ (getParam "label" atts) ++ "}"
       s4 = "{" ++ (getParam "title" atts) ++ "}"
       s5 = "{" ++ (getAttribs atts "" ["notationID", "label", "title"]) ++ "}\n"
       s6 = "\\end{" ++ (toLatexName name) ++ "}\n"
       str = s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ (fillLatex contents "") ++ s6
   in hasValue(EmacsContent [EditableText str])

makeMMiSSLatex ((Elem name atts contents), True) = 
   if (name /= "package") then hasError("Only a package element is allowed as root!")
     else 
       let first = head contents
           s1 = case first of
                 (CMisc (Comment str)) -> str
                 _ -> ""
           s2 = "\\begin{document}\n"
           s3 = "\\begin{" ++ (toLatexName name) ++ "}" 
           s4 = "[" ++ (getParam "notationID" atts) ++ "]"
           s5 = "{" ++ (getParam "label" atts) ++ "}"
           s6 = "{" ++ (getParam "title" atts) ++ "}"
           s7 = "{" ++ (getAttribs atts "" ["notationID", "label", "title"]) ++ "}\n"
           s8 = "\\end{" ++ (toLatexName name) ++ "}\n"
           s9 = "\\end{document}"
           str = s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7 ++ (fillLatex (tail contents) "") ++ s8 ++ s9
       in if (s1 /= "") 
            then hasValue(EmacsContent [EditableText str])
            else hasError ("The document contains no preamble!")


fillLatex :: [Content] -> String -> String

fillLatex [] inStr = inStr

fillLatex ((CElem (Elem "textFragment" atts contents)):cs) inStr = 
   let s1 = "\\begin{TextFragment}" 
       s2 = "[" ++ (getParam "notationID" atts) ++ "]"
       s3 = "{" ++ (getParam "label" atts) ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["notationID", "label"]) ++ "}\n"
       s5 = "\\end{TextFragment}\n"
       str = s1 ++ s2 ++ s3 ++ s4 ++ (fillLatex contents "") ++ s5
   in fillLatex cs (inStr ++ str)

fillLatex ((CElem (Elem "list" atts contents)):cs) inStr = 
   let s1 = "\\begin{List}" 
       s2 = "{" ++ (getParam "label" atts) ++ "}"
       listtype = getParam "type" atts
       s3 = "{" ++ listtype ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["type", "label"]) ++ "}\n"
       s5 = "\\end{List}\n"
       str = s1 ++ s2 ++ s3 ++ s4 ++ (fillLatex contents "") ++ s5
   in fillLatex cs (inStr ++ str)

fillLatex ((CElem (Elem "listItem" atts contents)):cs) inStr = 
   let s1 = "\\ListItem" 
       s2 = "{" ++ (getAttribs atts "" []) ++ "} "
       str = s1 ++ s2 ++ (fillLatex contents "")
   in fillLatex cs (inStr ++ str)

fillLatex ((CElem (Elem "emphasis" _ [CString _ str])):cs) inStr = 
   fillLatex cs (inStr ++ "\\Emphasis{" ++ str ++ "}\n") 

fillLatex ((CString _ str):cs) inStr = fillLatex cs (inStr ++ str)

fillLatex ((CMisc (Comment str)):cs) inStr = fillLatex cs (inStr ++ str)

fillLatex ((CElem (Elem name atts contents)):cs) inStr = 
  let s1 = "  \\begin{" ++ (toLatexName name) ++ "}" 
      s2 = "[" ++ (getParam "notationID" atts) ++ "]"
      s3 = "{" ++ (getParam "label" atts) ++ "}"
      s4 = "{" ++ (getParam "title" atts) ++ "}"
      s5 = "{" ++ (getAttribs atts "" ["notationID", "label", "title"]) ++ "}" ++ "\n"
      s6 = "  \\end{" ++ (toLatexName name) ++ "}\n"
  in fillLatex cs (inStr ++ s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ (fillLatex contents "") ++ s6)


getParam :: String -> [Attribute] -> String
getParam name atts = let value = lookup name atts
                     in case value of
                          Just(AttValue [(Left str)]) -> str
                          Nothing -> ""

-- ??? Generell alle Attribute einklammern?
getAttribs :: [Attribute] -> String -> [String] -> String
getAttribs [] str _ = if ((take 2 str) == ", ") 
                       then (drop 2 str)
                       else str  
getAttribs ((name, (AttValue [(Left value)])):as) str excludeList = 
   if (name `elem` excludeList)
     then getAttribs as str excludeList
     else getAttribs as (str ++ ", " ++ name ++ " = {" ++ value ++ "}") excludeList

                           
                                    
toLatexName :: String -> String
toLatexName name = maybe "" fst (find ((name ==) . snd) mmiss2EnvIds)
 

parseAndMakeMMiSSLatex :: SourceName -> IO ()
parseAndMakeMMiSSLatex name = do root <- parseMMiSSLatexFile name
                                 root1 <- return(coerceWithError root)
				 (EmacsContent ((EditableText str):_)) <- return(coerceWithError(makeMMiSSLatex (root1, True)))
				 putStrLn(str) 

                                    
