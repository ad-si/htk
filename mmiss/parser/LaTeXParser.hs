module LaTeXParser (
   mparse,
   parseMMiSSLatex, 
   -- :: Maybe MMiSSLatexPreamble -> String -> WithError Element
   -- Turn MMiSSLaTeX into an Element.   
   parseMMiSSLatexFile, -- :: SourceName -> IO (WithError Element)
   -- The same, for a file.
   makeMMiSSLatex,
   -- :: (Element, Bool) 
   -- -> WithError (EmacsContent TypedName, MMiSSLatexPreamble)
   -- Turns an Element into a MMiSSLaTeX source
   -- If the Bool is set, attaches a preamble.

   classifyLabelledTag, -- :: String -> Maybe Char
   -- Maps an Xml tag to its corresponding mini-type if it has one.
   -- (The mini-type is just something that identifies what sort of
   -- include the String needs.)

   fromIncludeStr, --  String -> Char
   fromIncludeStrOpt, -- String -> Maybe Char
   toIncludeStr, -- Char -> String
   -- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
   -- the corresponding includeXXX command.
   parseAndMakeMMiSSLatex,  -- SourceName -> Bool -> IO ()
   -- parseAndMakeMMiSSLatex parses a MMiSSLatex file and converts it back to MMiSSLatex
   -- combination of parseMMiSSLatexFile and makeMMiSSLatex 
   showElement, -- WithError Element -> String
   mapLabelledTag,
   MMiSSLatexPreamble
   )
 where

-- module LaTeXParser where

import IOExts
import List
import Parsec
import Char
import XmlTypes
import Pretty hiding (char, spaces)
import qualified XmlPP as PP
import Computation hiding (try)
import ParsecError
import EmacsContent
-- import EmacsEdit(TypedName)

type MMiSSLatexPreamble = String
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



plainTextAtoms = [("Table","table"), ("Glossaryentry", "glossaryEntry"), ("Bibentry", "bibEntry")] ++
                 [("Figure", "figure"), ("ProgramFragment", "programFragment")] ++
                 [("Clause", "clause"), ("Step", "step"), ("Authorentry", "authorEntry")]

envsWithText = [("Section", "section"), ("Paragraph", "paragraph"), ("Abstract", "abstract")] ++
               [("Introduction", "introduction"),  ("Summary", "summary"), ("Program","program")] ++
	       [("Exercise","exercise"), ("Example","example"),  ("Definition","definition")] ++
               [("Theory","theory"), ("Theorem", "theorem"), ("Development","development")] ++
               [("Proof","proof"), ("Script","script"), ("List", "list"), ("ListItem", "listItem")] ++
               [("Conjecture", "conjecture"), ("Lemma", "lemma"), ("Corollary", "corollary")] ++
               [("Assertion", "assertion"), ("TextFragment","textFragment")]

envsWithoutText = [("Package", "package")]

includeCommands =  [("IncludeGroup", "includeGroup"), ("IncludeUnit", "includeUnit")] ++
                   [("IncludeAtom", "includeAtom"), ("IncludeTextFragment","includeTextFragment")] ++
		   [("IncludeSection", "includeSection"), ("IncludeAbstract","includeAbstract")] ++
                   [("IncludeIntroduction", "includeIntroduction"), ("IncludeSummary","includeSummary")] ++
                   [("IncludeFormalUnit", "includeFormalUnit"), ("IncludeAtom","includeAtom")] ++
                   [("IncludeConceptualUnit", "includeConceptualUnit")] ++
                   [("IncludeConceptualAtom", "includeConceptualAtom")] ++
                   [("IncludeProgramFragment","includeProgramFragment")] ++
                   [("IncludeProgram", "includeProgram"), ("IncludeClause","includeClause")] ++
                   [("IncludeTheory", "includeTheory"), ("IncludeStep","includeStep")] ++
                   [("IncludeProof", "includeProof"), ("IncludeScript","includeScript")] ++
                   [("IncludeDevelopment", "includeDevelopment")]

linkAndRefCommands = [("Link", "link"), ("ForwardLink","link"), ("Reference","reference")] ++
                     [("ForwardReference", "reference")]

embeddedElements = [("Emphasis","emphasis"), ("IncludeTextFragment","includeTextFragment")] ++
		   [("Link","link") , ("Define", "define"), ("Reference", "reference")] ++
                   [("ForwardLink","link"), ("ForwardReference", "reference")]


-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

mmiss2EnvIds = plainTextAtoms ++ envsWithText ++ envsWithoutText ++ linkAndRefCommands

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
               key <- try(many1(noneOf " ,=}")) <?> "attribute name"
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
               in fmap Other (many (noneOf s))


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

mEnvParams "TextFragment" = 
   do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
      spaces
      labelId <-  try(between (char '{') (char '}') idParser)
                   <?> "{labelID}{title}{attribute-list} for Environment <TextFragment>"   
      spaces
      attributes <- try(between (char '{') (char '}') attParser)
		     <?> "{attribute-list} for Environment <TextFragment>"   
      if formId == "" 
        then return(MParams Nothing labelId "" attributes)
        else return(MParams (Just(formId)) labelId "" attributes)

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
      do labelId <-  try(between (char '{') (char '}') idParser)
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
         statusAtt <- if (isPrefixOf "Forward" id) 
			then return (("status", "absent"))
			else return (("status", "present"))
         attributes <- return (attributes ++ [statusAtt])
         return (LParams [(SingleParam optFrag '['), (SingleParam (Other labelId) '{')] (Just(attributes)))
 
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
command = do c <- try (many1 (noneOf "\\\v\f\t\r\n{[(}]} \""))
             str1 <- try (many (oneOf " \t"))
             l <- lParams c []
             if (c `elem` (map fst (embeddedElements ++ linkAndRefCommands ++ includeCommands)))
               then return (Command c l)
               else return (Command (c ++ str1) l)


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

parseMMiSSLatex :: Maybe MMiSSLatexPreamble -> String -> WithError Element

parseMMiSSLatex preamble s = 
  let result = parse (latexDoc []) "" s
  in case result of
--     Right ast  -> trace s (makeXML peamble ast)
       Right ast  -> makeXML preamble ast
       Left err -> hasError (concat (map messageString (errorMessages(err))))


parseMMiSSLatexFile :: Maybe MMiSSLatexPreamble -> SourceName -> IO (WithError Element)
parseMMiSSLatexFile p s = do result <- parseFromFile (latexDoc []) s
 		             case result of
			       Right ast  -> return(makeXML p ast)
			       Left err -> return(hasError (concat (map messageString (errorMessages(err)))))

parseAndShow :: SourceName -> IO ()
parseAndShow s = do result <- parseFromFile (latexDoc []) s
 		    case result of
			Right ast  -> print ast
			Left err -> print err


showElement :: WithError Element -> String
showElement e = coerceWithError (mapWithError (render . PP.element) e)

showElement1 :: Content -> String
showElement1 (CElem e) = (render . PP.element) e

makeXML :: Maybe MMiSSLatexPreamble -> Frag -> WithError Element
makeXML preamble frag = case preamble of
                          Nothing -> findFirstEnv [frag] [] False
                          Just(str) -> findFirstEnv [frag] [(Other str)] False

{-- findFirstEnv geht den vom Parser erzeugten abstrakten Syntaxbaum (AST) durch und erzeugt einen
    XML-Baum. Die Funktion sucht innerhalb des Environments 'Root' (vom Parser obligatorisch
    erzeugte Wurzel jedes AST) nach dem ersten MMiSSLatex-Environment oder dem ersten 'document'
    Env. Findet es zuerst das document-Env., wird die Suche nach einem MMiSSLatex-Env. in dessen
    Kindern fortgesetzt. Gleichzeitig wird das Aufsammeln von Praeambel-Fragmenten beendet.
    Findet es innerhalb des document-Envs ein package-Env, dann wird dieses als Wurzel des
    erzeugten XML-Baums eingesetzt und mittels 'makeContent' der Inhalt des packages in XML umgesetzt.
    Am Ende wird das Praeambel-Element an die package sowie an alle Elemente des XML-Baums (jeweils
    ans Ende des Element-Contents) angehaengt. Enthaelt die document-Umgebung nach dem Package-Env.
    noch MMiSSLatex- oder Latex-Fragmente, so werden diese ignoriert.
    Trifft die Funktion innerhalb des Root- oder document-Fragments auf eine andere MMiSSLatex-Umgebung
    als 'package', dann wird angenommen, dass es sich um ein Teildokument haldelt, das ausgecheckt wurde.
    Der Inhalt dieses Elementes (das kann eine Unit sein, aber auch ein Atom - z.B. ein TextFragment)
    wird als Wurzel fuer den erzeugten XML-Baum eingesetzt und der Inhalt der Umgebung XML umgewandelt.
    Hier wird jedoch keine Praeambel hinzugefuegt, weil es sich nur um ein Teildokument handelt,
    das mit Praeambel ausgecheckt wurde.
--}
    

findFirstEnv :: [Frag] -> [Frag] -> Bool -> WithError Element

findFirstEnv ((Env "Root" _ fs):[]) preambleFS _  = findFirstEnv fs preambleFS True
findFirstEnv ((Env "document" _ fs):_) preambleFs _ = findFirstEnv fs preambleFs False
findFirstEnv ((Env "Package" ps fs):_) preambleFs _ = 
  let atts = makeAttribs ps "Package"
      content = makeContent fs NoText "package"
      preamblePI = (CMisc (PI ("LaTeXParser:preamble", makeTextElem preambleFs)))
      newContent = mapWithError (map (addPreamble preamblePI)) content
  in mapWithError (Elem "package" atts) (mapWithError (append preamblePI) newContent)

findFirstEnv ((Env name ps fs):rest) preambleFs beforeDocument = 
  if (name `elem` (map fst (plainTextAtoms ++ envsWithText ++ envsWithoutText))) then
    let c1 = makeContent [(Env name ps fs)] (detectTextMode name) "Root"
        preamblePI = (CMisc (PI ("LaTeXParser:preamble", makeTextElem preambleFs)))
	c2 = mapWithError (map (addPreamble preamblePI)) c1
        c3 = fromWithError c2
    in case c3 of
         (Left str) -> hasError(str)
         (Right cs) -> if ((genericLength cs) == 0) 
		         then hasError("Internal Error: no XML content could be genereated for topmost Env. '" ++ name ++ "'")
                         else let ce = head cs
                              in case ce of 
			           (CElem e) -> hasValue(e)
			           _ -> hasError("Internal Error: no XML element could be genereated for topmost Env. '" ++ name ++ "'")
    else if (name `elem` (map fst mmiss2EnvIds)) 
           then findFirstEnv rest preambleFs beforeDocument
           else if (not beforeDocument)
	          -- Env is in document-Env but is not MMiSSLatex: pull out content and search there as well:
                  then findFirstEnv (fs ++ rest) preambleFs False
                  -- We are before document-Env. and it is no MMiSSLatex: add to preamble-Fragments
                  else findFirstEnv rest (preambleFs ++ [(Env name ps fs)]) True
 
findFirstEnv (f:fs) preambleFs True = findFirstEnv fs (preambleFs ++ [f]) True
findFirstEnv (f:fs) preambleFs False = findFirstEnv fs preambleFs False
findFirstEnv [] _ _  = hasError("No root environment ('package' or some other env.) found!")           


addPreamble :: Content -> Content -> Content
addPreamble preambleElem (CElem (Elem name atts content))  = 
  if ((name `elem` (map snd includeCommands)) || (name == "figure")) 
    then CElem (Elem name atts content)
    else CElem (Elem name atts ((map (addPreamble preambleElem) content) ++ [preambleElem]))
addPreamble _ e = e


makeContent :: [Frag] -> Textmode -> String -> WithError [Content]

makeContent [] _ _ = hasValue([])
makeContent (f:frags) NoText parentEnv = 
   case f of
--  TODO: (EscapedChar c) -> Merken, da er sonst verschwindet (und das kann ein \\-Zeilenumbruch sein)
     (Other str) -> if ((length (filter (not . (`elem` "\n ")) str) == 0) ||
			((head str) == '%'))
                      then mapWithError ([(CMisc (Comment str))] ++) (makeContent frags NoText parentEnv)
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
                      in myConcatWithError 
                             (cElemListWithError ename (makeAttribs ps name)
	                                               (makeContent fs (detectTextMode name) name))
                             (makeContent frags NoText parentEnv)
                 else  -- No MMiSS-Env.
                   let begin = hasValue([(CMisc (Comment 
                                                 ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ "\n")))])
                       end =  hasValue([(CMisc (Comment ("\\end{" ++ name ++ "}")))])
	   	       body = (makeContent fs NoText parentEnv)
                       whole = myConcatWithError (myConcatWithError begin body) end
                   in myConcatWithError whole (makeContent frags NoText parentEnv)
--		   myConcatWithError (makeContent fs NoText parentEnv) (makeContent frags NoText parentEnv)
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
	       in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))]))
                                    (makeContent frags NoText parentEnv)
	  else makeContent frags NoText parentEnv
     _ -> makeContent frags NoText parentEnv

makeContent (f:frags) TextAllowed "List" = 
   case f of
     (Other str) -> myConcatWithError (hasValue([(CMisc (Comment str))])) (makeContent frags TextAllowed "List")
     (Env name ps fs) -> 
        if (name `elem` (map fst mmiss2EnvIds))
          then hasError("Environment '" ++ name ++ "' is not allowed in lists. Wrap it up with a ListItem.")
          else  -- No MMiSS-Env.
	    myConcatWithError (makeContent fs TextAllowed "List") (makeContent frags TextAllowed "List")
     (Command "ListItem" ps) -> 
         let (content, restFrags) = makeListItem ps frags []
         in myConcatWithError (hasValue([content])) (makeContent restFrags TextAllowed "List")
     _ -> makeContent frags TextAllowed "List"

makeContent (f:frags) TextAllowed parentEnv = 
   case f of
     (EscapedChar c) ->  let cstr = if (c == '\\') then "\\" else [c]
                         in myConcatWithError (hasValue([(CMisc (Comment cstr))])) 
                                              (makeContent frags TextAllowed parentEnv)
     (Other str) -> if ((head str) == '%')
                      then myConcatWithError (hasValue([(CMisc (Comment str))])) 
                                             (makeContent frags TextAllowed parentEnv)
	              else if (genericLength (filter (not . isSpace) str) > 0)
                             then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
                                  in  myConcatWithError (hasValue([content])) 
				                        (makeContent restFrags TextAllowed parentEnv)
                             else makeContent frags TextAllowed parentEnv

     (Env name ps fs) -> 
       if (name `elem` (map fst plainTextAtoms))
         then
           let ename = maybe "" snd (find ((name ==) . fst) plainTextAtoms)
           in  myConcatWithError
                  (cElemListWithError ename (makeAttribs ps name) (hasValue([CString True (makeTextElem fs)])))
                  (makeContent frags TextAllowed parentEnv)
         else
           if (name == "TextFragment")
	     then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                  in  myConcatWithError (hasValue([(makeTextFragment parentEnv ename (Just(ps)) fs [])])) 
                                        (makeContent frags TextAllowed parentEnv)
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in myConcatWithError (cElemListWithError ename (makeAttribs ps name)
	                                                             (makeContent fs (detectTextMode name) name))
                                           (makeContent frags TextAllowed parentEnv)
                 else  -- No MMiSS-Env.
                   let begin = hasValue([(CMisc (Comment 
                                                 ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ "\n")))])
                       end =  hasValue([(CMisc (Comment ("\\end{" ++ name ++ "}")))])
	   	       body = (makeContent fs TextAllowed parentEnv)
                       whole = myConcatWithError (myConcatWithError begin body) end
                   in myConcatWithError whole (makeContent frags TextAllowed parentEnv)
--		   myConcatWithError (makeContent fs TextAllowed parentEnv) 
--                                     (makeContent frags TextAllowed parentEnv)
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
	       in myConcatWithError (cElemListWithError ename (makeIncludeAttribs ps) (hasValue([])))
                                                        (makeContent frags TextAllowed parentEnv)
          else if (name `elem` (map fst embeddedElements))
                 then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
                      in  myConcatWithError (hasValue([content])) 
	   	                            (makeContent restFrags TextAllowed parentEnv)
                 else myConcatWithError (hasValue([CMisc (Comment ("\\" ++ name ++ (lparamsToString ps)))]))
                                        (makeContent frags TextAllowed parentEnv)
--                 else makeContent frags TextAllowed parentEnv


makeTextElem :: [Frag] -> String 

makeTextElem [] = ""
makeTextElem (f:fs) = 
  case f of
    (EscapedChar c) -> "\\" ++ (if (c == '\\') then "\\" else [c]) ++ (makeTextElem fs)
    (Other str) -> str ++ (makeTextElem fs)
    (Command name ps) -> "\\" ++ name ++ (lparamsToString ps) ++ (makeTextElem fs)
    (Env name ps content) -> "\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ "\n" ++ 
				(makeTextElem content) ++
			     "\\end{" ++ name ++ "}" ++ (makeTextElem fs) 


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

--                             else makeNamelessTextFragment parentEnv (fs ++ frags) textFrags   -- Latex-Env.
                             else makeNamelessTextFragment parentEnv frags (textFrags ++ [f])  -- Latex-Env.
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
    (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                       in makeTextFragment parentEnv name params frags (content ++ [(CString True ("\\" ++ cstr))])
    (Command "Emphasis" ps) -> let newElem = (CElem (Elem "emphasis" [] [(CString True (getEmphasisText ps))]))
			        in makeTextFragment parentEnv name params frags (content ++ [newElem]) 
    (Command "IncludeTextFragment" ps) -> 
         let newElem = CElem (Elem "includeTextFragment" (makeIncludeAttribs ps) [])
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Link" ps) ->
         let newElem = CElem (Elem "link" (makeLinkAttribs ps) (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "ForwardLink" ps) -> 
         let newElem = CElem (Elem "link" (makeLinkAttribs ps) (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Reference" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "ForwardReference" ps) -> 
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command "Define" ps) ->
         let newElem = CElem (Elem "define" (makeDefineAttribs ps) (getDefineText ps))
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Command cname ps) ->
         let newElem = CMisc (Comment ("\\" ++ cname ++ (lparamsToString ps)))
 	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Env ename ps fs) -> 
         let begin = [CMisc (Comment ("\\begin{" ++ ename ++ "}\n" ++ (lparamsToString ps)))]
             end =  [CMisc (Comment ("\\end{" ++ ename ++ "}"))]
             (CElem (Elem _ _ c)) = makeTextFragment "TextFragment" name params fs []
         in makeTextFragment parentEnv name params frags (content ++ begin ++ c ++ end)
    _ -> makeTextFragment parentEnv name params frags content                         



makeListItem :: Params -> [Frag] -> [Content] -> (Content, [Frag])

makeListItem params [] contentList = 
   ((CElem (Elem "listItem" (makeListItemAttribs params) contentList)), [])

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
                             (contentList ++ [(CElem (Elem ename (makeAttribs ps name)
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
     (Command "ListItem" ps) -> 
        ((CElem (Elem "listItem" (makeListItemAttribs params) contentList)), (f:frags))
{--
     (Command name ps) -> 
        if (name `elem` (map fst embeddedElements))
          then let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
          else let newElem = CMisc (Comment ("\\" ++ name ++ (lparamsToString ps)))
               in makeListItem params frags (contentList ++ [newElem])
--}
     _ -> makeListItem params frags contentList


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
detectTextMode name = if (name `elem` (map fst (envsWithText ++ plainTextAtoms))) then TextAllowed
                       else NoText


makeAttribs :: Params -> String -> [Attribute]

makeAttribs ps name = 
  if (name `elem` (map fst includeCommands))
    then makeIncludeAttribs ps
    else if (name == "TextFragment")
           then makeTextFragmentAttribs (Just(ps))
	   else if (name `elem` ["Link", "ForwardLink"])
	          then makeLinkAttribs ps
		  else if (name `elem` ["Reference", "ForwardReference"]) 
		         then makeRefAttribs ps
			 else if (name == "Define")
			        then makeDefineAttribs ps
				else case ps of
				       (MParams Nothing label title atts) ->
					   let p1 = if (label == "") then []
						      else [("label", (AttValue [Left label]))]
					       p2 = if (title == "") then []
 						      else [("title", (AttValue [Left title]))]
					   in p1 ++ p2 ++ (map convertAttrib atts)
				       (MParams (Just formID) label title atts) ->
					   let p1 = [("notation", (AttValue [Left formID]))]
					       p2 = if (label == "") then []
						      else [("label", (AttValue [Left label]))]
					       p3 = if (title == "") then []
						      else [("title", (AttValue [Left title]))]
					   in p1 ++ p2 ++ p3 ++ (map convertAttrib atts)
makeAttribs _ _ = []
  

makeIncludeAttribs :: Params -> [Attribute]
makeIncludeAttribs (LParams ((SingleParam (Other labelId) _):[]) (Just(atts))) =
  [("included", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeIncludeAttribs (LParams ((SingleParam (Other labelId) _):[]) Nothing) =
  [("included", (AttValue [Left labelId]))]
makeIncludeAttribs _ = []


makeTextFragmentAttribs :: Maybe(Params) -> [Attribute]
makeTextFragmentAttribs Nothing = []
makeTextFragmentAttribs (Just (MParams formId labelId _ atts)) =
  let a1 = case formId of
	      Just(str) -> [("notation", (AttValue [Left str]))]
              Nothing -> []
      a2 = if (labelId == "") then [] else [("label", (AttValue [Left labelId]))]
      a3 = (map convertAttrib atts)
  in a1 ++ a2 ++ a3


makeListItemAttribs :: Params -> [Attribute]
makeListItemAttribs (LParams _ (Just(atts))) = (map convertAttrib atts)
makeListItemAttribs _ = []


makeLinkAttribs :: Params -> [Attribute]
makeLinkAttribs (LParams ps (Just(atts))) =
  [("linked", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeLinkAttribs (LParams ps Nothing) =
  [("linked", (AttValue [Left labelId]))]
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeLinkAttribs _ = []


makeRefAttribs :: Params -> [Attribute]
makeRefAttribs (LParams ps (Just(atts))) =
  [("referenced", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeRefAttribs (LParams ps Nothing) =
  [("referenced", (AttValue [Left labelId]))]
  where
    (SingleParam (Other labelId) _) = genericIndex ps 1
makeRefAttribs _ = []


makeDefineAttribs :: Params -> [Attribute]
makeDefineAttribs (LParams ((SingleParam (Other labelId) _):_) atts) =
  let attList = case atts of
                  Just(a) -> (map convertAttrib a)
                  Nothing -> []
  in [("label", (AttValue [Left labelId]))] ++ attList
makeDefineAttribs _ = []


-- getLinkText erwartet die Params eines Link oder Reference-Elementes und extrahiert daraus
-- den LinkText, der im ersten SingleParam steht und leer sein kann.

getLinkText :: Params -> [Content]
getLinkText (LParams ((SingleParam f '['):_) _) =
  let str = makeTextElem [f]
  in if (str == "") then []
       else [(CString True str)]


getDefineText :: Params -> [Content]
getDefineText  (LParams ps _ ) =
  if (text == "") 
    then []
    else [(CString True text)]
  where
    (SingleParam (Other text) _) = genericIndex ps 1


convertAttrib :: (String, String) -> Attribute
convertAttrib (l, r) = ((attNameToXML l), AttValue [Left r])


getEmphasisText :: Params -> String
getEmphasisText (LParams [] _) = ""
getEmphasisText (LParams ((SingleParam (Other s) _):ps) _) = s



{-- makeMMiSSLatex erzeugt aus einem XML-Element die zugehoerige MMiSSLatex-Repraesentation.
--}

makeMMiSSLatex :: (Element, Bool) -> WithError ((EmacsContent (String, Char)),  MMiSSLatexPreamble)

makeMMiSSLatex ((Elem name atts content), preOut) = 
  let items = fillLatex [(CElem (Elem name atts content))] []
      lastElem = last content
      preambleText = case lastElem of
                        (CMisc (PI (_ ,str))) -> str
                        _ -> ""
      preambleItem = [(EditableText preambleText)]
  in if preOut 
       then 
         let beginDocument = [EditableText "\\begin{document}\n"]
             endDocument = [EditableText "\\end{document}"]
         in hasValue((EmacsContent (preambleItem ++ beginDocument ++ items ++ endDocument)), preambleText)
       else hasValue((EmacsContent items), preambleText)


fillLatex :: [Content] -> [EmacsDataItem (String, Char)] -> [EmacsDataItem (String, Char)]

fillLatex [] l = l

fillLatex ((CElem (Elem "textFragment" atts contents)):cs) inList = 
   let s1 = "\n\\begin{TextFragment}" 
       s2 = "[" ++ (getParam "notation" atts) ++ "]"
       s3 = "{" ++ (getParam "label" atts) ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["notation", "label"]) ++ "}"
       s5 = "\\end{TextFragment}\n"
       items = if ((s2 ++ s3 ++ s4) == "[]{}{}") 
                 then (fillLatex contents [])
                 else [(EditableText (s1 ++ s2 ++ s3 ++ s4))] ++ (fillLatex contents []) 
                      ++ [(EditableText s5)]
   in fillLatex cs (inList ++ items)

fillLatex ((CElem (Elem "list" atts contents)):cs) inList = 
   let s1 = "\n\\begin{List}" 
       s2 = "{" ++ (getParam "label" atts) ++ "}"
       listtype = getParam "type" atts
       s3 = "{" ++ listtype ++ "}"
       s4 = "{" ++ (getAttribs atts "" ["type", "label"]) ++ "}\n"
       s5 = "\\end{List}\n"
       items = [(EditableText (s1 ++ s2 ++ s3 ++ s4))] ++ (fillLatex contents [])
               ++ [(EditableText s5)]
   in fillLatex cs (inList ++ items)

fillLatex ((CElem (Elem "listItem" atts contents)):cs) inList = 
   let s1 = "\\ListItem" 
       s2 = "{" ++ (getAttribs atts "" []) ++ "} "
       items = [EditableText (s1 ++ s2)] ++ (fillLatex contents [])
   in fillLatex cs (inList ++ items)

fillLatex ((CElem (Elem "emphasis" _ ((CString _ str):_))):cs) inList = 
   fillLatex cs (inList ++ [EditableText ("\\Emphasis{" ++ str ++ "}")]) 

fillLatex ((CElem (Elem ('i':'n':'c':'l':'u':'d':'e':unit) atts _)):cs) inList = 
   let labelId = getParam "included" atts
       item = [EmacsLink (labelId, (fromIncludeStr unit))]
   in fillLatex cs (inList ++ item)

fillLatex ((CElem (Elem name atts contents)):cs) inList
  | (name `elem` (map snd linkAndRefCommands)) =  
    let forwardStr = if ((getParam "status" atts) == "absent")
                       then "Forward"
                       else ""
        s1 = "\\" ++ forwardStr ++ (elemNameToLaTeX name) 
        str = if (length(contents) == 0) then "" 
                 else let c = head contents
                      in case c of
                           (CString _ body) -> body
                           _ -> ""
        s2 = "[" ++ str ++ "]"
        s3 = "{" ++  if (name == "link") then (getParam "linked" atts) ++ "}" else  (getParam "referenced" atts) ++ "}"
        s4 = "{" ++ (getAttribs atts "" ["linked", "referenced", "status"]) ++ "}"
        items = [(EditableText (s1 ++ s2 ++ s3 ++ s4))]
    in fillLatex cs (inList ++ items)
  | (name == "define") =
    let s1 = "\\" ++ (elemNameToLaTeX name)
        s2 = "{" ++ (getParam "label" atts) ++ "}"
        s3 = "{" ++ (getParam "body" atts) ++ "}"
        s4 = "{" ++ (getAttribs atts "" ["label", "body"]) ++ "}"
        items = [(EditableText (s1 ++ s2 ++ s3 ++ s4))]
    in fillLatex cs (inList ++ items)
        
fillLatex ((CString _ str):cs) inList = fillLatex cs (inList ++ [(EditableText str)])

fillLatex ((CMisc (Comment str)):cs) inList = fillLatex cs (inList ++ [(EditableText str)])

fillLatex ((CElem (Elem name atts contents)):cs) inList = 
  let s1 = "\n\\begin{" ++ (elemNameToLaTeX name) ++ "}" 
      s2 = "[" ++ (getParam "notation" atts) ++ "]"
      s3 = "{" ++ (getParam "label" atts) ++ "}"
      s4 = "{" ++ (getParam "title" atts) ++ "}"
      s5 = "{" ++ (getAttribs atts "" ["notation", "label", "title"]) ++ "}\n"
      s6 = "\n\\end{" ++ (elemNameToLaTeX name) ++ "}\n"
      items = [(EditableText (s1 ++ s2 ++ s3 ++ s4 ++ s5))] ++ (fillLatex contents []) 
              ++ [(EditableText s6)]
  in fillLatex cs (inList ++ items)

fillLatex (c:cs) inList = fillLatex cs inList


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
     else getAttribs as (str ++ ", " ++ attNameToLatex(name) ++ " = {" ++ value ++ "}") excludeList                


attNameToLatex :: String -> String
attNameToLatex "xml:lang" = "Language"
attNameToLatex name = [(toUpper (head name))] ++ (tail name)


attNameToXML :: String -> String
attNameToXML "Language" = "xml:lang"
attNameToXML name = [(toLower (head name))] ++ (tail name)


elemNameToLaTeX :: String -> String
elemNameToLaTeX name = maybe "" fst (find ((name ==) . snd) 
                                          (mmiss2EnvIds ++ embeddedElements ++ includeCommands))


myConcatWithError :: WithError [a] -> WithError [a] -> WithError [a]

myConcatWithError l m = mapWithError (uncurry (++)) (pairWithError l m)


cElemListWithError:: String -> [Attribute] -> WithError [Content] -> WithError [Content]

cElemListWithError name atts c = concatWithError [(mapWithError CElem (mapWithError (Elem name atts) c))]
  

-- Maps an Xml tag to its corresponding mini-type if it has one.
-- (The mini-type is just something that identifies what sort of
-- include the String needs.)
classifyLabelledTag :: String -> Maybe Char
classifyLabelledTag str = fromIncludeStrOpt (mapLabelledTag str)

-- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
-- the corresponding includeXXX command.
toIncludeStr :: Char -> String
toIncludeStr 'G' = "Group"
toIncludeStr 'U' = "Unit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "TextFragment"
toIncludeStr 'S' = "Section"
toIncludeStr 'a' = "Abstract"
toIncludeStr 'I' = "Introduction"
toIncludeStr 's' = "Summary"
toIncludeStr 'F' = "FormalUnit"
toIncludeStr 'C' = "ConceptualAtom"
toIncludeStr 'p' = "ProgramFragment"
toIncludeStr 'P' = "Program"
toIncludeStr 'c' = "Clause"
toIncludeStr 't' = "Theory"
toIncludeStr 'x' = "Step"
toIncludeStr 'y' = "Proof"
toIncludeStr 'z' = "Script"
toIncludeStr 'D' = "Development"
toIncludeStr 'E' = "ConceptualUnit"
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"


---
-- fromIncludeStrOpt
-- and also handles the case where the first letter is lower-cased.
fromIncludeStrOpt :: String -> Maybe Char
fromIncludeStrOpt "Group" = Just 'G'
fromIncludeStrOpt "Unit" = Just 'U'
fromIncludeStrOpt "Atom" = Just 'A'
fromIncludeStrOpt "TextFragment" = Just 'T'
fromIncludeStrOpt "Section"         = Just 'S'                
fromIncludeStrOpt "Abstract"        = Just 'a'        
fromIncludeStrOpt "Introduction"    = Just 'I'        
fromIncludeStrOpt "Summary"         = Just 's'        
fromIncludeStrOpt "FormalUnit"      = Just 'F'        
fromIncludeStrOpt "ConceptualAtom"  = Just 'C'        
fromIncludeStrOpt "ProgramFragment" = Just 'p'         
fromIncludeStrOpt "Program"         = Just 'P'        
fromIncludeStrOpt "Clause"          = Just 'c'        
fromIncludeStrOpt "Theory"          = Just 't'        
fromIncludeStrOpt "Step"            = Just 'x'        
fromIncludeStrOpt "Proof"           = Just 'y'        
fromIncludeStrOpt "Script"          = Just 'z'        
fromIncludeStrOpt "Development"     = Just 'D' 
fromIncludeStrOpt "ConceptualUnit"  = Just 'E' 
fromIncludeStrOpt (c : cs) | Char.isLower c 
   = fromIncludeStrOpt (toUpper c : cs)
fromIncludeStrOpt _ = Nothing


fromIncludeStr :: String -> Char
fromIncludeStr str = case fromIncludeStrOpt str of
   Just c -> c
   Nothing -> error 
    ("MMiSSDTDAssumptions.fromIncludeStr - bad include string"++str)

---
-- Map tags to the name of their corresponding include element (minus
-- "include")
mapLabelledTag :: String -> String
mapLabelledTag s = 
   case s of
      "package" -> "Group"
      "section" -> "Section"
      "paragraph" -> "Group"
      "view" -> "Group"
      "example" -> "ConceptualUnit"
      "exercise" -> "ConceptualUnit"
      "definition" -> "ConceptualUnit"
      "theorem" -> "FormalUnit"
      "conjecture" -> "FormalUnit"
      "lemma" -> "FormalUnit"
      "corollary" -> "FormalUnit"
      "assertion" -> "FormalUnit"
      "list" -> "Unit"
      "table" -> "ConceptualAtom"
      "figure" -> "ConceptualAtom"
      "glossaryEntry" -> "ConceptualAtom"
      "bibEntry" -> "ConceptualAtom"
      _ -> mapUpper s
   where
      mapUpper [] = []
      mapUpper (c : cs) = toUpper c : cs      


parseAndMakeMMiSSLatex :: SourceName -> Bool -> IO ()
parseAndMakeMMiSSLatex name pre = 
  do root <- parseMMiSSLatexFile Nothing name
     root1 <- return(coerceWithError root)
     ((EmacsContent l), pre) <- return(coerceWithError(makeMMiSSLatex (root1, True)))
     putStrLn (concat (map getStrOfEmacsDataItem l))

{--
parseMakeParse :: SourceName -> IO (WithError Element)

parseMakeParse name = do root <- parseMMiSSLatexFile name
                         root1 <- return(coerceWithError root)
			 (EmacsContent l) <- return(coerceWithError(makeMMiSSLatex (root1, True)))
			 str <- return (concat (map getStrOfEmacsDataItem l))
                         return(parseMMiSSLatex str)
--}

getStrOfEmacsDataItem :: EmacsDataItem (String, Char) -> String

getStrOfEmacsDataItem (EditableText str) = str
getStrOfEmacsDataItem (EmacsLink (str,c)) = str ++ [c]                                   


append :: a -> [a] -> [a]
append x xs = xs ++ [x]


