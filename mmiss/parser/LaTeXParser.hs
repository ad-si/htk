
module LaTeXParser where

import Parsec
import Char
import XmlTypes
import Pretty hiding (char, spaces)
import qualified XmlPP as PP
import Computation hiding (try)
import ParsecError

type EnvId = String
type Command = String
type FormId = String
type LabelId = String
type Title = String
type Attributes = [(String, String)]
type Other = String

data SingleParam = SingleParam Frag Char    deriving Show

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
            | LParams [SingleParam]                       -- Parameter of LateX-Envs
              deriving Show

-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

mmiss2EnvIds = ["Package","Section","Paragraph","View","Example","Exercise","Definition"] ++
               ["TextFragment","Table","Figure","GlossaryEntry", "Program","Theory","Theorem"] ++
               ["Conjecture","Lemma","Corollary","Assertion","Development","Proof","Script"] ++
               ["ProgramFragment","Clause","Step","Bibentry","Authorentry","List","Listitem"] ++
               ["Emphasis"]

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
envParams id =  if (id `elem` mmiss2EnvIds) 
                    then if (id == "list")
                           then mListParams id
			   else mEnvParams id 
                    else lParams [] <?> ("Parameters for LaTeX-Environment <" ++ id ++ ">")


-- mListParams erkennt die Parameter, die zu einer MMiSSLatex-Umgebung gehoeren 
-- (mit Ausnahme von List-Umgebungen. Dies sind: 
-- [FormalismID] {LabelID} {Title} {Attributes}

mEnvParams :: String -> GenParser Char st Params
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


-- mListParams erkennt die Parameter, die zu einer MMiSSLatex-List-Umgebung gehoeren.
-- Die Unterscheidung zu mEnvParams ist notwendig, da List-Umgebungen kein Title-
-- Parameter haben.

mListParams :: String -> GenParser Char st Params
mListParams id = do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
		    spaces
		    labelId <-  try(between (char '{') (char '}') idParser)
			        <?> "{labelID}{title}{attribute-list} for Environment <" ++ id ++ ">"   
		    spaces
                    listType <- try(between (char '{') (char '}') listTypeParser)
                                <?> "listtype 'itemize' or 'enumeration' for Environment <list>"
	            spaces
		    attributes <- try(between (char '{') (char '}') attParser)
				 <?> "{attribute-list} for Environment <" ++ id ++ ">"   
		    if formId == "" 
		      then return(MParams Nothing labelId "" (("Type",listType):attributes))
		      else return(MParams (Just(formId)) labelId "" (("Type",listType):attributes))
	

-- lParams erkennt Parameter eines Latex-Commands. Diese koennen in normalen, geschweiften
-- oder eckigen Klammern eingeschlossen sein. Es wird nicht geprueft, ob die Parameter
-- und ihre Reihenfolge den Latex-Definitionen entsprechen.
	    
lParams :: [SingleParam] -> GenParser Char st Params
lParams l = do p <- try ( genParam '{' '}' )
               lParams ((SingleParam p '{'):l)  
            <|>  do p <- try ( genParam '[' ']' )
                    lParams ((SingleParam p '['):l)
            <|>  do p <- try ( genParam '(' ')' )
                    lParams ((SingleParam p '('):l)
            <|>  return (LParams (reverse l))

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
command = do c <- many1 (noneOf "\\\v\f\t\r\n{[( ")
             skipMany (oneOf " \t")
             l <- lParams [] 
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
             return (Other ("%" ++ s))


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
	      <|> return (Env "Root" (LParams []) (reverse l))


-- Haupt-Funktion (eigentlich main)

mparse :: SourceName -> IO ()
mparse fname = do result <- parseFromFile (latexDoc []) fname
                  case result of 
                      Left err -> print err
                      Right f -> print f



{-- Main function: Parses the given MMiSSLatex-string and returns an Element which holds the
    XML-structure.  --}

parseMMiSSLatex :: SourceName -> IO (WithError Element)

parseMMiSSLatex s = do result <- parseFromFile (latexDoc []) s
 		       case result of
			   Right ast  -> return(makeXML ast)
			   Left err -> return(hasError (concat (map messageString (errorMessages(err)))))


showElement :: WithError Element -> String

showElement e = coerceWithError (mapWithError (render . PP.element) e)


makeXML :: Frag -> WithError Element

makeXML frag = let (rootElem, frags) = coerceWithError(findFirstPackage [frag])
	       in  hasValue(rootElem)


findFirstPackage :: [Frag] -> WithError (Element, [Frag])

findFirstPackage 
    ((Env "Root" _ fs):[])    = findFirstPackage fs
findFirstPackage 
    ((Env "document" _ fs):_) = findFirstPackage fs
findFirstPackage 
    ((Env "Package" ps fs):_) = hasValue(((Elem "package" (makeAttribs ps) 
					    (makeContent fs), fs)))
findFirstPackage 
    (f:fs)                    = findFirstPackage fs
findFirstPackage 
    []                        = hasError("No topmost 'package' element found!")           


makeContent :: [Frag] -> [Content]

makeContent (f:frags) = 
   case f of
     (Env name ps fs) -> 
       if (name `elem` mmiss2EnvIds) 
	 then [(CElem (Elem name (makeAttribs ps)
	    (makeContent fs)))] ++ (makeContent frags)
         else (makeContent fs) ++ (makeContent frags)
     (Command "Emphasis" ps) -> 
	(CElem (Elem "emphasis" [] 
                    [CString True (getEmphasisText ps)])):(makeContent frags)
     _ -> makeContent frags 
makeContent [] = []



--  MParams (Maybe FormId) LabelId Title Attributes
makeAttribs :: Params -> [Attribute]

makeAttribs 
  (MParams Nothing label title atts) = [("label", (AttValue [Left label]))] ++
                                       [("title", (AttValue [Left title]))] ++
                                       (map convertAttribs atts)
makeAttribs 
  (MParams (Just formID) label title atts) = [("notationID", (AttValue [Left formID]))] ++
			 		     [("label", (AttValue [Left label]))] ++
                                             [("title", (AttValue [Left title]))] ++
                                             (map convertAttribs atts)
makeAttribs 
  _ = []
  

convertAttribs :: (String, String) -> Attribute

convertAttribs (l, r) = (l, AttValue [Left r])


getEmphasisText :: Params -> String

getEmphasisText (LParams []) = ""
getEmphasisText (LParams ((SingleParam (Other s) _):ps)) = s
 

