
module LaTeXParser where

import Parsec
-- import ParsecToken
import Char

-- type LaTeXText = [Frag]

type EnvId = String
type Command = String
type FormId = String
type LabelId = String
type Title = String
type Attributes = [String]
type Other = String

-- data ComParams = ComParams [String]     deriving (Eq,Show)
-- data OptParams = OptParams [String]     deriving (Eq, Show)
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

mmiss2EnvIds = ["package","section","paragraph","view","example","exercise","definition"] ++
               ["textfragment","table","figure","glossaryentry", "program","theory","theorem"] ++
               ["conjecture","lemma","corollary","assertion","development","proof","script"] ++
               ["programfragment","clause","step","bibentry","authorentry"]

----------------------------------------------
--
-- Hier beginnen die Parser
--
----------------------------------------------

backslash = char '\\'

idParser = many (noneOf "}]")

-- idParser1 c = many (noneOf (c:[]))

commaSep p = p `sepBy` (oneOf ",")


-- paramParser erkennt mit Komma getrennte Strings innerhalb von optionalen oder zwingenden
-- Befehlsargumenten.

paramParser :: GenParser Char st [String]
paramParser = commaSep (many (noneOf ",]}"))


-- attParser :: GenParser Char st Attributes

{- attParser as = do manyTill anyChar (try (oneOf ",}"))
                  a <- sepAttParser
                  oneOf ",}"
                  attParser (a:as)
               <|> return (reverse as)
-}

attParser = do  spaces
                char '{'
                l <- sepBy (many (noneOf ",}")) (char ',')
                char '}'
                return l
            <|> do string "{}"
                   return []
            <|> return []

{-
-- sepAttParser :: String -> GenParser Char st (String, String)
sepAttParser = do key <- many (noneOf "=")
                  key <- head l
                  val <- head (tail l)
                  return (key, val)       
-}

{- genParam parses an arbitrary Parameter of a LaTeX-Environment or Command (no Parameter of
   MMiSSLaTeX-Environments - these are handled by "attParser".
   Because LaTeX-Environment-Parameters can be delimited by either '{' or '[' or '('
   (the latter since LaTeX 2e I think), the l und r parameters to genParam
   hold the delimiter character, to look out for. e.g. l = '{', r = '}'  -}

genParam l r = between (char l) (char r) frag <?> ("fragment between " ++ [l] ++ " and " ++ [r])



-- other generiert Text-Fragmente

other = fmap Other (many1 (noneOf "\\%"))


-- begin erkennt den Namen einer Umgebung (id)

begin = do  try (string "begin")
            spaces
            c <- between (char '{') (char '}') idParser
            spaces
            return(c)
                            
-- end  ueberprueft, ob als naechstes ein \end{id} in der Source auftaucht.

end id = do backslash
            string "end"
            spaces 
            c <- between (char '{') (char '}') idParser
            return(c)

continue l id = (try (end id) >>= 
                       (\ x -> if x == id then return l else fail ("no matching end-Tag for <" ++ id ++ ">")))
                <|> do f <- frag
                       continue (f:l) id

-- beginBlock erkennt den Start einer Umgebung (\begin{id}) und parst mittels
-- 'continue' den Inhalt der Umgebung. 'continue' achtet darauf, dass es zu der 
-- id aus dem begin-Block auch ein \end{id}-Block gibt.  
               
beginBlock :: GenParser Char st Frag
beginBlock = do id <- begin
                params <- envParams id <?> "Parameter for Environment"
                l <- continue [] id
                return (Env id params (reverse l))


envParams id = if ((map toLower id) `elem` mmiss2EnvIds) 
                 then mEnvParams <?> ("Parameters for MMiSS-Environment <" ++ id ++ ">")
                 else lParams [] <?> ("Parameters for LaTeX-Environment <" ++ id ++ ">")


mEnvParams = do formId  <-  option "" (try ( between (char '[') (char ']') idParser))
                spaces
                labelId <-  between (char '{') (char '}') idParser
		spaces
                title <-   between (char '{') (char '}') idParser
                spaces
 		attributes <- attParser <?> "attributes for MMiSS-Env"
                if formId == "" 
                  then return(MParams Nothing labelId title attributes)
                  else return(MParams (Just(formId)) labelId title attributes)


lParams l = do p <- try ( genParam '{' '}' )
               lParams ((SingleParam p '{'):l)  
            <|>  do p <- try ( genParam '[' ']' )
                    lParams ((SingleParam p '['):l)
            <|>  do p <- try ( genParam '(' ')' )
                    lParams ((SingleParam p '('):l)
            <|>  return (LParams (reverse l))


continueAdhocEnv l = do try (char '}')
                        return l
                     <|> do f <- frag
                            continueAdhocEnv (f:l) 
                         

-- adhocEnvironment erkennt Umgebungen ohne Namen: ...{text}...

adhocEnvironment = do char '{'
                      l <- continueAdhocEnv [] <?> "closing } for unnamed environment"
                      return (Env "" (LParams []) (reverse l))



-- command erkennt LaTeX-Kommandos. Escaped letters wie \{, \$ etc. werden hier 
-- nicht erkannt, sondern von escapedChar geparst.
command :: GenParser Char st Frag
command = do c <- many1 (noneOf "\\\v\f\t\r\n{[ ")
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

comment = do char '%'
             s <- manyTill anyChar (try newline)
             return (Other ("%" ++ s))


frag :: GenParser Char st Frag
frag = comment
       <|> do backslash
              beginBlock <|> escapedChar <|> command <|> return (Other "\\")
--     <|> adhocEnvironment
       <|> other


-- doc ist der Haupt-Parser. Er sammelt die Fragmente der Root-Ebene ein und kapselt
-- sie in eine virtuelle Root-Umgebung, damit eine saubere Baum-Struktur entsteht. 

doc :: [Frag] -> GenParser Char st Frag
doc l =  do f <- frag <?> "Fragment"
            doc (f:l)
         <|> return (Env "Root" (LParams []) (reverse l))


-- Haupt-Funktion (eigentlich main)

mparse :: SourceName -> IO ()
mparse fname = do result <- parseFromFile (doc []) fname
                  case result of 
                      Left err -> print err
                      Right f -> print f




