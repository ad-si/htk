module Main(main) where

import HTk
import Concurrent
import Events
import FileDialog
import System

import Posix
import PrelHandle
import IO
import IOExts
import PrelIOBase
import PrelForeign

type Path = String
type Name = String
type Output   = String
type Run  = Bool
type Grapher = Maybe ChildProcess
data CATSData = CATSData Path Name Parameters Output Run Grapher

data Parameters = Parameters { p_i_aterm       :: String,
                               p_i_static      :: String,
                               p_i_bin         :: String,
                               p_i_basic_empty :: Int,
                               p_o_tex         :: String,
                               p_o_binary      :: String,
                               p_o_xml         :: String,
                               p_o_aterm       :: String,
                               p_o_e_encoding  :: Int,
                               p_o_e_output    :: Int,
                               p_o_e_other     :: Int,
                               p_graph         :: Int,
                               p_quiet         :: String
                             }
			     
data Switches = Switches { s_i_aterm       :: TkVariable String,
                           s_i_static      :: TkVariable String,
                           s_i_bin         :: TkVariable String,
                           s_i_basic_empty :: TkVariable Int,
                           s_o_tex         :: TkVariable String,
                           s_o_binary      :: TkVariable String,
                           s_o_xml         :: TkVariable String,
                           s_o_aterm       :: TkVariable String,
                           s_o_e_encoding  :: TkVariable Int,
                           s_o_e_output    :: TkVariable Int,
                           s_o_e_other     :: TkVariable Int,
                           s_graph         :: TkVariable Int,
                           s_quiet         :: TkVariable String
                         }


main :: IO()
main =
 do
  win <- initHTk [text "CATS-GUI"]
    
  sw <- createMenuBar win

  (htk_destr, _) <- bindSimple win Destroy
  sync htk_destr

---
-- the receive channel:
-- the Int represents the command, the String always a filename to be added
-- the String is only important for one command
-- commands:
-- 0-n: an other entry in the listbox(lb) was selected, we need to update the switches-menu
-- -1 : one of the switches was modified, we need to save this change to the current lb-selection
-- -2 : a new file is to be added, now the String is of importance
-- -3 : delete the selected file from our list
-- -4 : the "run"-command was selected, now we need to call CATS
-- ... perhaps more to come (save,load...)
dataserver :: Channel (Int,String) -> [CATSData] -> Switches -> ListBox [String] -> Editor String -> Editor String -> IO()
dataserver rec cdata sw lb ed at = 
 do
  (command,file) <- sync(receive rec)
  case command of
   -4 -> do
          newcdata <- callCATS (tail cdata)
          sync(noWait(send rec (1,"")))
          dataserver rec ((head cdata):newcdata) sw lb ed at
   -3 -> do
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed at
           (Just (sel:xs)) -> do
	                       case (sel::Int) of
                                0 -> dataserver rec cdata sw lb ed at
                                _ -> do
                                      newcdata <- removeFile cdata (sel::Int)
                                      newnames <- cdataToStrList newcdata
                                      lb # value newnames	  
                                      dataserver rec newcdata sw lb ed at
   -2 -> do
          newcdata <- addFile cdata (seperateFilePath file "")
	  newnames <- cdataToStrList newcdata
          lb # value newnames	  
	  dataserver rec newcdata sw lb ed at
   -1 -> do 
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed at
           (Just (sel:xs)) -> do
                               newcdata <- updateCDATA cdata sw (sel::Int)
                               dataserver rec newcdata sw lb ed at
   _  -> do
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed at
           (Just (sel:xs)) -> do
                               updateSwitches cdata sw (sel::Int) ed at
			       newdata <- callVisiulizer (tail cdata) ((sel::Int)-1)
                               dataserver rec cdata sw lb ed at

callCATS :: [CATSData] -> IO ([CATSData])
callCATS [] = return ([])
callCATS ((CATSData path file p t r g):xs) =
 do
  let params = parametersToString p
  putStrLn("Calling file: "++file++"... "++params)
  maybechild <- executeChild "cats" [params,(path++file)]
  case maybechild of
   Nothing -> do
               putStrLn("error during fork")
               rest <- callCATS xs
               return ((CATSData path file p t r g):rest)
   Just child -> do
               out <- readFD (child # stdoutFd)
               err <- readFD (child # stderrFd)
               finishChildProcess child               	       
               rest <- callCATS xs
               return ((CATSData path file p (t++out++err) True g):rest)

---
-- the first element of the CATSData list is seperated because the default parameters need
-- to be copyed to the new element (namedrecord??)
addFile :: [CATSData] -> (String,String) -> IO ([CATSData])
addFile ((CATSData pa fi p d r g):xs) (path,file) = return (((CATSData pa fi p d r g):xs)++[(CATSData path file p "" r g)])

removeFile :: [CATSData] -> Int -> IO ([CATSData])
removeFile [] _ = return ([])          -- just to make sure
removeFile (x:xs) 0 = return (xs)
removeFile (x:xs) i = 
 do
  rest <- removeFile xs (i-1)
  return (x:rest)

createMenuBar :: HTk -> IO (Switches)
createMenuBar win =
 do
  m <- createMenu win False []
  win # menu m

  file <- createMenuCascade m [text "File"]
  menufile <- createMenu win False []
  file # menu menufile
  
  ---
  -- a channel for commication
  toServ <- newChannel

  ---
  -- create switches-menu
  sw <- createSwitchesMenu win m toServ

  ---
  -- create list to contain CATSData
  params <- switchesToParameters sw
  let cdata = [(CATSData "" "Defaults" params "Nothing here!" False Nothing)]

  ---
  -- Create list containing filenames
  box <- newHBox win []
  pack box [Fill Both, Side AtLeft, Expand On]
  
  namelist <- cdataToStrList cdata
  lb <- newListBox box [size (15,10), value namelist] :: IO (ListBox [String])
  pack lb [Side AtLeft, Fill Both, Expand On]
  
  scb1 <- newScrollBar box []
  pack scb1 [Side AtLeft, Fill Y, Expand Off]
  lb # scrollbar Vertical scb1

  (press,_) <- bindSimple lb (ButtonPress (Just (BNo 1)))
  spawnEvent (forever (press >> always (do 
                                         (Just (sel:xs)) <- getSelection lb
                                         sync(send toServ (sel::Int,""))
					 )))

  textboxes <- newVFBox box []
  pack textboxes [Fill Both, Side AtRight, Expand On]
  ---
  -- add textarea for CATS-output
  edbox <- newHBox textboxes []
  pack edbox [Fill X, Side AtTop, Expand On]
  ed <- newEditor edbox [size (50,10)] :: IO (Editor String)
  pack ed [Side AtLeft, Fill X, Expand On]
  
  scb2 <- newScrollBar edbox []
  pack scb2 [Side AtRight, Fill Y, Expand Off]
  ed # scrollbar Vertical scb2

  ---
  -- add textarea for generated aterm-files
  atbox <- newHBox textboxes []
  pack atbox [Fill Both, Side AtTop, Expand On]
  at <- newEditor atbox [size (50,20)] :: IO (Editor String)
  pack at [Side AtLeft, Fill Both, Expand On]
  
  scb3 <- newScrollBar atbox []
  pack scb3 [Side AtRight, Fill Y, Expand Off]
  at # scrollbar Vertical scb3

  ---
  -- create file-menu
  createFileMenu win menufile toServ
  
  ---
  -- we need something to keep all information together.
  -- there we can get or receive informations 
  forkIO (dataserver toServ cdata sw lb ed at)
  
  return (sw)

startFileDialog :: Channel (Int,String) -> IO ()
startFileDialog toServ =
 do
  pwd <- getEnv "PWD"
  selev <- fileDialog "Open file" pwd
  file  <- sync selev
  case file of 
   Just fp -> ssend toServ (-2,fp)
   _ -> return ()

createFileMenu :: HTk -> Menu -> Channel (Int,String) -> IO ()
createFileMenu win menufile toServ =
 do
  run <- createMenuCommand menufile [text "Run CATS"]
  clickedrun <- clicked run
  spawnEvent (forever (clickedrun >> always (ssend toServ (-4,""))))
  
  createMenuSeparator menufile []

  add <- createMenuCommand menufile [text "Add File"]
  clickedadd <- clicked add
  spawnEvent (forever (clickedadd >> always (startFileDialog toServ)))

  del <- createMenuCommand menufile [text "Remove File"]
  clickeddel <- clicked del
  spawnEvent (forever (clickeddel >> always (ssend toServ (-3,""))))
  
  createMenuSeparator menufile []

  open <- createMenuCommand menufile [text "Open"]
  clickedopen <- clicked open
  spawnEvent (forever (clickedopen >> always (click "\"Open\"")))
  
  save <- createMenuCommand menufile [text "Save"]
  clickedsave <- clicked save
  spawnEvent (forever (clickedsave >> always (click "\"Save\"")))
  
  createMenuSeparator menufile []

  quit <- createMenuCommand menufile [text "Quit"]
  clickedquit <- clicked quit
  spawnEvent (forever (clickedquit >> always (destroy win)))

  return ()

createSwitchesMenu :: HTk -> Menu -> Channel (Int,String) -> IO (Switches)
createSwitchesMenu win m toServ =
 do
  ---
  -- Create the TkVariables needed
  v_input_aterm                 <- createTkVariable "0"
  v_input_static                <- createTkVariable "1"
  v_input_bin                   <- createTkVariable "1"
  v_input_basic_empty           <- createTkVariable (0 :: Int)
  v_output_tex                  <- createTkVariable "0"
  v_output_binary               <- createTkVariable "1"
  v_output_xml                  <- createTkVariable "0"
  v_output_aterm                <- createTkVariable "1"
  v_environment_encoding        <- createTkVariable (3::Int)
  v_environment_output          <- createTkVariable (1::Int)
  v_environment_other           <- createTkVariable (0::Int)
  v_graph                       <- createTkVariable (3::Int)
  v_quiet                       <- createTkVariable "0"

  ---
  -- Create the Menu-Layout
  ------------------------------- NEW SUBMENU ENVIRONMENT ----------------------------------
  input <- createMenuCascade m [text "Input"]
  menu_input <- createMenu win True []
  input # menu menu_input
  
  output <- createMenuCascade m [text "Output/Spec"]
  menu_output <- createMenu win True []
  output # menu menu_output

  environment <- createMenuCascade m [text "Environment"]
  menu_environment <- createMenu win True []
  environment # menu menu_environment

  graph <- createMenuCascade m [text "Graph"]
  menu_graph <- createMenu win True []
  graph # menu menu_graph

  switches <- createMenuCascade m [text "Other"]
  menu_switches <- createMenu win True []
  switches # menu menu_switches

  ---
  -- Create the Menu-Buttons
  ------------------------------------ INPUT ---------------------------------------------
  input_aterm  <- createMenuCheckButton menu_input [text "ATerm",  variable v_input_aterm]
  input_static <- createMenuCheckButton menu_input [text "Static", variable v_input_static]
  input_bin    <- createMenuCheckButton menu_input [text "Bin",    variable v_input_bin]
  input_basic  <- createMenuRadioButton menu_input [text "Basic",  value (0 :: Int), variable v_input_basic_empty]
  input_empty  <- createMenuRadioButton menu_input [text "Empty",  value (1 :: Int), variable v_input_basic_empty]
  ------------------------------------ OUTPUT ---------------------------------------------
  output_tex    <- createMenuCheckButton menu_output [text "LaTex",    variable v_output_tex]
  output_binary <- createMenuCheckButton menu_output [text "Binary", variable v_output_binary]
  output_xml    <- createMenuCheckButton menu_output [text "XML",    variable v_output_xml]
  output_aterm  <- createMenuCheckButton menu_output [text "ATerm",  variable v_output_aterm]  
  -------------------------------- ENVIRONMENT-ENCODING ------------------------------------
  environment_casenv  <- 
   createMenuRadioButton menu_environment [text "CasEnv", value (0 :: Int), 
                              	           variable v_environment_encoding]
  environment_fcasenv <- 
   createMenuRadioButton menu_environment [text "FCasEnv", value (1 :: Int), 
                                           variable v_environment_encoding]
  environment_hcasenv <- 
   createMenuRadioButton menu_environment [text "HCasEnv", value (2 :: Int), 
                                           variable v_environment_encoding]
  environment_noenv   <- 
   createMenuRadioButton menu_environment [text "NoEnv", value (3 :: Int), 
                                           variable v_environment_encoding]
  createMenuSeparator menu_environment []
  ----------------------------- ENVIRONMENT-OUTPUT-FORMAT -----------------------------------
  environment_text  <-
   createMenuRadioButton menu_environment [text "text", value (0 :: Int), variable v_environment_output]
  environment_aterm <-
   createMenuRadioButton menu_environment [text "aterm", value (1 :: Int), variable v_environment_output]
  environment_latex <-
   createMenuRadioButton menu_environment [text "latex", value (2 :: Int), variable v_environment_output]
  environment_xml   <-
   createMenuRadioButton menu_environment [text "xml", value (3 :: Int), variable v_environment_output]
  createMenuSeparator menu_environment []
  ------------------------------ ENVIRONMENT-OTHER ------------------------------------------
  environment_subpcfol <-
   createMenuRadioButton menu_environment [text "SubPCFOL", value (0 :: Int), variable v_environment_other]
  environment_subcfol <-
   createMenuRadioButton menu_environment [text "SubCFOL", value (1 :: Int), variable v_environment_other]
  environment_cfol <-
   createMenuRadioButton menu_environment [text "CFOL", value (2 :: Int), variable v_environment_other]
  environment_sol <-
   createMenuRadioButton menu_environment [text "SOL", value (3 :: Int), variable v_environment_other]
  environment_cfol1 <-
   createMenuRadioButton menu_environment [text "CFOL1", value (4 :: Int), variable v_environment_other]
  environment_sol1 <-
   createMenuRadioButton menu_environment [text "SOL1", value (5 :: Int), variable v_environment_other]
  ------------------------------------ GRAPH ---------------------------------------------
  graph_davinci <-
   createMenuRadioButton menu_graph [text "davinci", value (0 :: Int), variable v_graph]
  graph_ps <-
   createMenuRadioButton menu_graph [text "ps", value (1 :: Int), variable v_graph]
  graph_dot <-
   createMenuRadioButton menu_graph [text "dot", value (2 :: Int), variable v_graph]
  graph_nograph <- 
   createMenuRadioButton menu_graph [text "no graph", value (3 :: Int), variable v_graph]
  ------------------------------------ QUIET ---------------------------------------------
  quiet <-
   createMenuCheckButton menu_switches [text "quiet", variable v_quiet]
  cl_quiet <- clicked quiet


  ---
  -- Create Events
  cl_input_aterm <- clicked input_aterm
  cl_input_static <- clicked input_static
  cl_input_bin <- clicked input_bin
  cl_input_basic <- clicked input_basic
  cl_input_empty <- clicked input_empty  
  let input_event = cl_input_aterm +> cl_input_static +> cl_input_bin +> cl_input_basic +> cl_input_empty 
  cl_output_tex <- clicked output_tex
  cl_output_binary <- clicked output_binary
  cl_output_xml <- clicked output_xml
  cl_output_aterm <- clicked output_aterm
  let output_main_event = cl_output_tex +> cl_output_binary +> cl_output_xml +> cl_output_aterm 
  cl_environment_casenv <- clicked environment_casenv
  cl_environment_fcasenv <- clicked environment_fcasenv
  cl_environment_hcasenv <- clicked environment_hcasenv
  cl_environment_noenv <- clicked environment_noenv
  let encoding_event = cl_environment_casenv +> cl_environment_fcasenv 
                              +> cl_environment_hcasenv +> cl_environment_noenv 
  cl_environment_text <- clicked environment_text
  cl_environment_aterm <- clicked environment_aterm
  cl_environment_latex <- clicked environment_latex
  cl_environment_xml <- clicked environment_xml
  let format_event = cl_environment_text +> cl_environment_aterm 
                            +> cl_environment_latex +> cl_environment_xml 
  cl_environment_subpcfol <- clicked environment_subpcfol
  cl_environment_subcfol <- clicked environment_subcfol
  cl_environment_cfol <- clicked environment_cfol
  cl_environment_sol <- clicked environment_sol
  cl_environment_cfol1 <- clicked environment_cfol1
  cl_environment_sol1 <- clicked environment_sol1
  let environment_other = cl_environment_subpcfol +> cl_environment_subcfol 
                                 +> cl_environment_cfol +> cl_environment_sol 
				 +> cl_environment_cfol1 +> cl_environment_sol1
  let output_event = output_main_event +> encoding_event +> format_event +> environment_other
  cl_graph_davinci <- clicked graph_davinci
  cl_graph_ps <- clicked graph_ps
  cl_graph_dot <- clicked graph_dot
  cl_graph_nograph <- clicked graph_nograph
  let graph_event = cl_graph_davinci +> cl_graph_ps +> cl_graph_dot +> cl_graph_nograph 
  let switches_event = input_event +> output_event +> graph_event +> cl_quiet
  
  spawnEvent (forever (switches_event >> always (ssend toServ (-1,""))))
  
  ------------------------------------ END GRAPH ---------------------------------------------
  ------------------------------------ END QUIET ---------------------------------------------
  return (Switches v_input_aterm v_input_static v_input_bin v_input_basic_empty v_output_tex v_output_binary v_output_xml v_output_aterm v_environment_encoding v_environment_output v_environment_other v_graph v_quiet)

  

seperateFilePath :: String -> String -> (String,String)
seperateFilePath [] f = ("",f)
seperateFilePath p f | last p == '/' = (p,f)
                     | otherwise = seperateFilePath (init p) ((last p):f)

updateCDATA :: [CATSData] -> Switches -> Int -> IO ([CATSData])
updateCDATA [] _ _ = return ([])
updateCDATA ((CATSData path file _ i r g):xs) sw 0 = 
 do
  newparams <- switchesToParameters sw
  return (((CATSData path file newparams i r g):xs))
updateCDATA (x:xs) sw sel =
 do
  rest <- updateCDATA xs sw (sel-1)
  return ((x:rest))


updateSwitches :: [CATSData] -> Switches -> Int ->  Editor String -> Editor String -> IO ()
updateSwitches [] _ _ _ _ = return ()
updateSwitches ((CATSData path name p t r g):xs) sw 0 ed at = 
 do
  ed # value t
  parametersToSwitches p sw
  if ((p # p_o_aterm) == "1" && r && (p # p_o_binary) == "0")
   then
    do
     either_hdl <- (IO.try (openFile (path++(noending name)++".trm") ReadMode))
     case either_hdl of
      Left e    -> do
                    at # value "no .trm file"
                    return()
      Right hdl -> do
                    str <- hGetContents hdl
                    at # value str
                    return()
   else
    do
     at # value ""
     return()
updateSwitches (x:xs) sw sel ed at = updateSwitches xs sw (sel-1) ed at

noending :: String -> String
noending []  = ""
noending str | last str == '.' = init str
             | otherwise = noending (init str)


switchesToParameters :: Switches -> IO (Parameters)
switchesToParameters s =
 do
  i1 <- readTkVariable (s # s_i_aterm)
  i2 <- readTkVariable (s # s_i_static)
  i3 <- readTkVariable (s # s_i_bin)
  i4 <- readTkVariable (s # s_i_basic_empty)
  o1 <- readTkVariable (s # s_o_tex) 
  o2 <- readTkVariable (s # s_o_binary)  
  o3 <- readTkVariable (s # s_o_xml)  
  o4 <- readTkVariable (s # s_o_aterm)  
  e1 <- readTkVariable (s # s_o_e_encoding)  
  e2 <- readTkVariable (s # s_o_e_output)  
  e3 <- readTkVariable (s # s_o_e_other)  
  g1 <- readTkVariable (s # s_graph)  
  q1 <- readTkVariable (s # s_quiet)  
  return (Parameters i1 i2 i3 i4 o1 o2 o3 o4 e1 e2 e3 g1 q1)
   
parametersToSwitches :: Parameters -> Switches -> IO ()
parametersToSwitches p s =
 do
  setTkVariable (s # s_i_aterm)       (p # p_i_aterm) 
  setTkVariable (s # s_i_static)      (p # p_i_static) 
  setTkVariable (s # s_i_bin)         (p # p_i_bin) 
  setTkVariable (s # s_i_basic_empty) (p # p_i_basic_empty) 
  setTkVariable (s # s_o_tex)         (p # p_o_tex) 
  setTkVariable (s # s_o_binary)      (p # p_o_binary) 
  setTkVariable (s # s_o_xml)         (p # p_o_xml) 
  setTkVariable (s # s_o_aterm)       (p # p_o_aterm) 
  setTkVariable (s # s_o_e_encoding)  (p # p_o_e_encoding) 
  setTkVariable (s # s_o_e_output)    (p # p_o_e_output) 
  setTkVariable (s # s_o_e_other)     (p # p_o_e_other) 
  setTkVariable (s # s_graph)         (p # p_graph) 
  setTkVariable (s # s_quiet)         (p # p_quiet) 
  return ()
  
cdataToStrList :: [CATSData] -> IO ([String])
cdataToStrList [] = return ([])
cdataToStrList ((CATSData _ s _ _ _ _):xs) = 
 do
  rest <- cdataToStrList xs
  return (s:rest)

click :: String -> IO ()
click nm = putStrLn ("clicked " ++ nm)

ssend :: Channel a -> a -> IO ()
ssend c m = sync(send c m)



---
-- building the cats paramerters
parametersToString :: Parameters -> String
parametersToString p = (buildInput p)++(buildOutput p)++(buildSpec p)++(buildEnv p)++(buildGraph (p # p_graph))++(buildQuiet (p # p_quiet))
  
buildInput :: Parameters -> String
buildInput p =
 " -input="++(buildIaterm (p # p_i_aterm))++","++(buildIstatic(p # p_i_static))++","
           ++(buildIbin (p # p_i_bin))++","++(buildIbasic (p # p_i_basic_empty))

buildOutput :: Parameters -> String
buildOutput p =
 " -output="++(buildOtex (p # p_o_tex))++","++(buildObinary (p # p_o_binary))++","++(buildOenv (p # p_o_e_encoding))++","++(buildOgraph(p # p_graph))

buildSpec :: Parameters -> String
buildSpec p =
 " -spec="++(buildOaterm (p # p_o_aterm))++","++(buildOxml (p # p_o_xml))
 
buildEnv :: Parameters -> String
buildEnv p =
 case (p # p_o_e_encoding) of
 3 -> ""
 _ -> " -env="++(buildEnvencoding (p # p_o_e_encoding))++","++(buildEnvOutput (p # p_o_e_output))++","++(buildEnvOther (p # p_o_e_other))

buildIaterm :: String -> String
buildIaterm p =
 if p == "1"
  then "aterm"
  else "casl"
   
buildIstatic :: String -> String
buildIstatic p =
 if p == "1"
  then "static"
  else "nostatic"
   
buildIbin :: String -> String
buildIbin p =
 if p == "1"
  then "bin"
  else "nobin"
   
buildIbasic :: Int -> String
buildIbasic p =
 if p == 0
  then "basic"
  else "empty"
   
buildOtex :: String -> String
buildOtex p =
 if p == "1"
  then "tex"
  else "notex"
   
buildObinary :: String -> String
buildObinary p =
 if p == "1"
  then "bin"
  else "nobin"
   
buildOaterm :: String -> String
buildOaterm p =
 if p == "1"
  then "aterm"
  else "noaterm"

buildOxml :: String -> String
buildOxml p =
 if p == "1"
  then "xml"
  else "noxml"   
   
buildOenv :: Int -> String
buildOenv p =
 case p of
  3 -> "noenv"
  _ -> "env" 

buildOgraph :: Int -> String
buildOgraph p =
 case p of
  3 -> "nograph"
  _ -> "graph" 

buildEnvencoding :: Int -> String
buildEnvencoding p = 
 case p of 
  0 -> "CasEnv"
  1 -> "FCasEnv"
  2 -> "HCasEnv"
  _ -> "noenv"

buildEnvOutput :: Int -> String
buildEnvOutput p =
 case p of
  0 -> "text"
  1 -> "aterm"
  2 -> "latex"
  3 -> "xml"
  _ -> "" --who knows?

buildEnvOther :: Int -> String
buildEnvOther p =
 case p of
  0 -> "SubPCFOL"
  1 -> "SubCFOL"
  2 -> "CFOL"
  3 -> "SOL"
  4 -> "CFOL1"
  5 -> "SOL1"
  _ -> "" --who knows?

buildGraph :: Int -> String
buildGraph p =
  case p of
   0 -> " -graph=davinci"
   1 -> " -graph=ps"
   2 -> " -graph=dot"
   _ -> ""

buildQuiet :: String -> String
buildQuiet p =
 if p == "1"
  then ""
  else " -noquiet"

---
-- strange, although newchild is (Just ChildProcess) next time we get here
-- its nothing !? somewhere its modified, but where?
callVisiulizer :: [CATSData] -> Int -> IO ([CATSData])
callVisiulizer [] _ = return ([])
callVisiulizer ((CATSData path name p t r g):xs) 0 =
 case g of 
  Just child -> do
                 progstat <- finishChildProcess child
                 case (p # p_graph) of
                  0 -> do --davinci
                        newchild <- executeChild "daVinci" [(path++(noending name)++".davinci")]
                        return ((CATSData path name p t r Nothing):xs)
                  1 -> do --ps
                        newchild <- executeChild "gv" [(path++(noending name)++".dot.ps")]
                        return ((CATSData path name p t r newchild):xs)  
                  2 -> do --dot(?)
                        return ((CATSData path name p t r g):xs)
                  _ -> do -- nograph
                        return ((CATSData path name p t r Nothing):xs)
  _ -> do
        case (p # p_graph) of
         0 -> do --davinci
               newchild <- executeChild "davinci" [(path++(noending name)++".davinci")]
               return ((CATSData path name p t r Nothing):xs)
         1 -> do --ps
               newchild <- executeChild "gv" [(path++(noending name)++".dot.ps")]
               return ((CATSData path name p t r newchild):xs)
         2 -> do --dot(?)
               return ((CATSData path name p t r g):xs)
         _ -> do -- nograph
               return ((CATSData path name p t r Nothing):xs)
callVisiulizer (x:xs) i =
 do
  rest <- callVisiulizer xs (i-1)
  return (x:rest)			

---------------------------------
-- needs to be somewhere else ---
---------------------------------
data ChildProcess = ChildProcess { pid      :: ProcessID,
		                   stdinFd  :: Fd,
                                   stdoutFd :: Fd,
                                   stderrFd :: Fd
                                 }

finishChildProcess :: ChildProcess -> IO (Maybe ProcessStatus)
finishChildProcess child =
 do
  fdClose (child # stdinFd)
  fdClose (child # stdoutFd)
  fdClose (child # stderrFd)
  getProcessStatus True False (child # pid)

 
executeChild :: String -> [String] -> IO(Maybe ChildProcess)
executeChild file arguments =
 do
  (readStdIn,writeStdIn)   <- createPipe
  (readStdOut,writeStdOut) <- createPipe
  (readStdErr,writeStdErr) <- createPipe
  pid <- forkProcess
  case pid of
   Just p -> do
              fdClose readStdIn
	      fdClose writeStdOut
	      fdClose writeStdErr
              threadWaitRead(fdToInt readStdOut)
              result <- fdRead readStdOut 1
              if (result /= ("#",1))
                  then
		     do
                      fdClose writeStdIn
                      fdClose readStdOut
                      fdClose readStdErr
                      return (Nothing)
                  else
                      return (Just(ChildProcess p writeStdIn readStdOut readStdErr))
   Nothing -> do
               fdClose writeStdIn
               fdClose readStdOut
               fdClose readStdErr
               nbytes <- fdWrite writeStdOut "#"
               if(nbytes/=1)
                  then
                     do		  
                      fdClose readStdIn
                      fdClose writeStdOut
                      fdClose writeStdErr
		      return (Nothing)
                  else
                     do		  
                      dupTo readStdIn stdInput
                      dupTo writeStdOut stdOutput
	              dupTo writeStdErr stdError
		      executeFile file True arguments Nothing
		      return (Nothing)

---
-- reads on a fd until an error occures (EOF)
readFD :: Fd -> IO (String);
readFD fd = 
 do 
  (s, bytes) <- catch (fdRead fd 1024) (\_ -> return ("", -1))
  case bytes of
   -1 -> do
          return ("")
   _  -> do
          t <- readFD fd
          return (s++t)
