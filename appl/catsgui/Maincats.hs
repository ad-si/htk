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
data CATSData = CATSData Path Name Parameters Output

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
dataserver :: Channel (Int,String) -> [CATSData] -> Switches -> ListBox [String] -> Editor String-> IO()
dataserver rec cdata sw lb ed = 
 do
  (command,file) <- sync(receive rec)
  case command of
   -4 -> do
          newcdata <- callCATS (tail cdata)
          sync(noWait(send rec (1,"")))
          dataserver rec ((head cdata):newcdata) sw lb ed
   -3 -> do
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed
           (Just (sel:xs)) -> do
	                       case (sel::Int) of
                                0 -> dataserver rec cdata sw lb ed
                                _ -> do
                                      newcdata <- removeFile cdata (sel::Int)
                                      newnames <- cdataToStrList newcdata
                                      lb # value newnames	  
                                      dataserver rec newcdata sw lb ed
   -2 -> do
          newcdata <- addFile cdata (seperateFilePath file "")
	  newnames <- cdataToStrList newcdata
          lb # value newnames	  
	  dataserver rec newcdata sw lb ed
   -1 -> do 
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed
           (Just (sel:xs)) -> do
                               newcdata <- updateCDATA cdata sw (sel::Int)
                               dataserver rec newcdata sw lb ed
   _  -> do
          selection <- getSelection lb
          case selection of
           Nothing -> dataserver rec cdata sw lb ed
           (Just (sel:xs)) -> do
                               updateSwitches cdata sw (sel::Int) ed
                               dataserver rec cdata sw lb ed

callCATS :: [CATSData] -> IO ([CATSData])
callCATS [] = return ([])
callCATS ((CATSData path file p t):xs) =
 do
  let params = parametersToString p
  putStrLn("Calling file "++file++"... "++params)
  maybechild <- executeChild "ls" ["."]
  case maybechild of
   Nothing -> do
               putStrLn("error during fork")
               rest <- callCATS xs
               return ((CATSData path file p t):rest)
   Just child -> do
               out <- readFD (child # stdoutFd)
               err <- readFD (child # stderrFd)
               finishChildProcess child
               case out of
                Nothing -> do
	                    case err of
			     Nothing -> do
                                         rest <- callCATS xs
                                         return ((CATSData path file p t):rest)
                             Just e  -> do
			                 rest <- callCATS xs
                                         return ((CATSData path file p (t++e)):rest)					 
                Just str -> do
                             case err of
                              Nothing -> do
                                          rest <- callCATS xs
                                          return ((CATSData path file p (t++str)):rest)
                              Just e  -> do
			                  rest <- callCATS xs
                                          return ((CATSData path file p (t++str++e)):rest)					 

---
-- the first element of the CATSData list is seperated because the default parameters need
-- to be copyed to the new element (namedrecord??)
addFile :: [CATSData] -> (String,String) -> IO ([CATSData])
addFile ((CATSData pa fi p d):xs) (path,file) = return (((CATSData pa fi p d):xs)++[(CATSData path file p "")])

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
  let cdata = [(CATSData "" "Defaults" params "Nothing here!")]

  ---
  -- Create list containing filenames
  box <- newHFBox win []
  pack box [Fill X, Fill Y, Side AtLeft]
  
  namelist <- cdataToStrList cdata
  lb <- newListBox box [value namelist] :: IO (ListBox [String])
  pack lb [Side AtLeft, Fill Y]
  
  scb1 <- newScrollBar box []
  pack scb1 [Side AtLeft, Fill Y]
  lb # scrollbar Vertical scb1

  (press,_) <- bindSimple lb (ButtonPress (Just (BNo 1)))
  spawnEvent (forever (press >> always (do 
                                         (Just (sel:xs)) <- getSelection lb
                                         sync(send toServ (sel::Int,""))
					 )))

  ---
  -- add textarea for CATS-output
  ed <- newEditor box [] :: IO (Editor String)
  pack ed [Side AtLeft, Fill Y, Expand On]
  
  scb2 <- newScrollBar box []
  pack scb2 [Side AtLeft, Fill Y]
  ed # scrollbar Vertical scb2

  ---
  -- create file-menu
  createFileMenu win menufile toServ
  
  ---
  -- we need something to keep all information together.
  -- there we can get or receive informations 
  forkIO (dataserver toServ cdata sw lb ed)
  
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
  v_output_binary               <- createTkVariable "0"
  v_output_xml                  <- createTkVariable "0"
  v_output_aterm                <- createTkVariable "0"
  v_output_environment_encoding <- createTkVariable (3::Int)
  v_output_environment_output   <- createTkVariable (3::Int)
  v_output_environment_other    <- createTkVariable (2::Int)
  v_graph                       <- createTkVariable (3::Int)
  v_quiet                       <- createTkVariable "0"

  ---
  -- Create the Menu-Layout
  ------------------------------- NEW SUBMENU ENVIRONMENT ----------------------------------
  switches <- createMenuCascade m [text "Switches"]
  menu_switches <- createMenu win True []
  switches # menu menu_switches

  input <- createMenuCascade menu_switches [text "Input"]
  menu_input <- createMenu win True []
  input # menu menu_input
  
  output <- createMenuCascade menu_switches [text "Output"]
  menu_output <- createMenu win True []
  output # menu menu_output

  output_environment <- createMenuCascade menu_output [text "Environment"]
  menu_output_environment <- createMenu win True []
  output_environment # menu menu_output_environment

  graph <- createMenuCascade menu_switches [text "Graph"]
  menu_graph <- createMenu win True []
  graph # menu menu_graph

  ---
  -- Create the Menu-Buttons
  ------------------------------------ INPUT ---------------------------------------------
  input_aterm  <- createMenuCheckButton menu_input [text "ATerm",  variable v_input_aterm]
  input_static <- createMenuCheckButton menu_input [text "Static", variable v_input_static]
  input_bin    <- createMenuCheckButton menu_input [text "Bin",    variable v_input_bin]
  input_basic  <- createMenuRadioButton menu_input [text "Basic",  value (0 :: Int), variable v_input_basic_empty]
  input_empty  <- createMenuRadioButton menu_input [text "Empty",  value (1 :: Int), variable v_input_basic_empty]
  ------------------------------------ OUTPUT ---------------------------------------------
  output_tex    <- createMenuCheckButton menu_output [text "Tex",    variable v_output_tex]
  output_binary <- createMenuCheckButton menu_output [text "Binary", variable v_output_binary]
  output_xml    <- createMenuCheckButton menu_output [text "XML",    variable v_output_xml]
  output_aterm  <- createMenuCheckButton menu_output [text "ATerm",  variable v_output_aterm]  
  -------------------------------- ENVIRONMENT-ENCODING ------------------------------------
  output_environment_casenv  <- 
   createMenuRadioButton menu_output_environment [text "CasEnv", value (0 :: Int), 
                                                  variable v_output_environment_encoding]
  output_environment_fcasenv <- 
   createMenuRadioButton menu_output_environment [text "FCasEnv", value (1 :: Int), 
                                                  variable v_output_environment_encoding]
  output_environment_hcasenv <- 
   createMenuRadioButton menu_output_environment [text "HCasEnv", value (2 :: Int), 
                                                  variable v_output_environment_encoding]
  output_environment_noenv   <- 
   createMenuRadioButton menu_output_environment [text "NoEnv", value (3 :: Int), 
                                                  variable v_output_environment_encoding]
  createMenuSeparator menu_output_environment []
  ----------------------------- ENVIRONMENT-OUTPUT-FORMAT -----------------------------------
  output_environment_text  <-
   createMenuRadioButton menu_output_environment [text "text", value (0 :: Int), variable v_output_environment_output]
  output_environment_aterm <-
   createMenuRadioButton menu_output_environment [text "aterm", value (1 :: Int), variable v_output_environment_output]
  output_environment_latex <-
   createMenuRadioButton menu_output_environment [text "latex", value (2 :: Int), variable v_output_environment_output]
  output_environment_xml   <-
   createMenuRadioButton menu_output_environment [text "xml", value (3 :: Int), variable v_output_environment_output]
  createMenuSeparator menu_output_environment []
  ------------------------------ ENVIRONMENT-OTHER ------------------------------------------
  output_environment_subpcfol <-
   createMenuRadioButton menu_output_environment [text "SubPCFOL", value (0 :: Int), variable v_output_environment_other]
  output_environment_subcfol <-
   createMenuRadioButton menu_output_environment [text "SubCFOL", value (1 :: Int), variable v_output_environment_other]
  output_environment_cfol <-
   createMenuRadioButton menu_output_environment [text "CFOL", value (2 :: Int), variable v_output_environment_other]
  output_environment_sol <-
   createMenuRadioButton menu_output_environment [text "SOL", value (3 :: Int), variable v_output_environment_other]
  output_environment_cfol1 <-
   createMenuRadioButton menu_output_environment [text "CFOL1", value (4 :: Int), variable v_output_environment_other]
  output_environment_sol1 <-
   createMenuRadioButton menu_output_environment [text "SOL1", value (5 :: Int), variable v_output_environment_other]
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
  cl_output_environment_casenv <- clicked output_environment_casenv
  cl_output_environment_fcasenv <- clicked output_environment_fcasenv
  cl_output_environment_hcasenv <- clicked output_environment_hcasenv
  cl_output_environment_noenv <- clicked output_environment_noenv
  let output_encoding_event = cl_output_environment_casenv +> cl_output_environment_fcasenv 
                              +> cl_output_environment_hcasenv +> cl_output_environment_noenv 
  cl_output_environment_text <- clicked output_environment_text
  cl_output_environment_aterm <- clicked output_environment_aterm
  cl_output_environment_latex <- clicked output_environment_latex
  cl_output_environment_xml <- clicked output_environment_xml
  let output_format_event = cl_output_environment_text +> cl_output_environment_aterm 
                            +> cl_output_environment_latex +> cl_output_environment_xml 
  cl_output_environment_subpcfol <- clicked output_environment_subpcfol
  cl_output_environment_subcfol <- clicked output_environment_subcfol
  cl_output_environment_cfol <- clicked output_environment_cfol
  cl_output_environment_sol <- clicked output_environment_sol
  cl_output_environment_cfol1 <- clicked output_environment_cfol1
  cl_output_environment_sol1 <- clicked output_environment_sol1
  let output_environment_other = cl_output_environment_subpcfol +> cl_output_environment_subcfol 
                                 +> cl_output_environment_cfol +> cl_output_environment_sol 
				 +> cl_output_environment_cfol1 +> cl_output_environment_sol1
  let output_event = output_main_event +> output_encoding_event +> output_format_event +> output_environment_other
  cl_graph_davinci <- clicked graph_davinci
  cl_graph_ps <- clicked graph_ps
  cl_graph_dot <- clicked graph_dot
  cl_graph_nograph <- clicked graph_nograph
  let graph_event = cl_graph_davinci +> cl_graph_ps +> cl_graph_dot +> cl_graph_nograph 
  let switches_event = input_event +> output_event +> graph_event +> cl_quiet
  
  spawnEvent (forever (switches_event >> always (ssend toServ (-1,""))))
  
  ------------------------------------ END GRAPH ---------------------------------------------
  ------------------------------------ END QUIET ---------------------------------------------
  return (Switches v_input_aterm v_input_static v_input_bin v_input_basic_empty v_output_tex v_output_binary v_output_xml v_output_aterm v_output_environment_encoding v_output_environment_output v_output_environment_other v_graph v_quiet)


seperateFilePath :: String -> String -> (String,String)
seperateFilePath [] f = ("",f)
seperateFilePath p f | last p == '/' = (p,f)
                     | otherwise = seperateFilePath (init p) ((last p):f)

updateCDATA :: [CATSData] -> Switches -> Int -> IO ([CATSData])
updateCDATA [] _ _ = return ([])
updateCDATA ((CATSData path file _ i):xs) sw 0 = 
 do
  newparams <- switchesToParameters sw
  return (((CATSData path file newparams i):xs))
updateCDATA (x:xs) sw sel =
 do
  rest <- updateCDATA xs sw (sel-1)
  return ((x:rest))

updateSwitches :: [CATSData] -> Switches -> Int ->  Editor String -> IO ()
updateSwitches [] _ _ _= return ()
updateSwitches ((CATSData _ _ p t):xs) sw 0 ed = 
 do
  ed # value t
  parametersToSwitches p sw
updateSwitches (x:xs) sw sel ed = updateSwitches xs sw (sel-1) ed

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
cdataToStrList ((CATSData _ s _ _):xs) = 
 do
  rest <- cdataToStrList xs
  return (s:rest)

click :: String -> IO ()
click nm = putStrLn ("clicked " ++ nm)

ssend :: Channel a -> a -> IO ()
ssend c m = sync(send c m)


parametersToString :: Parameters -> String
parametersToString p = (buildInput p)++(buildOutput p)++(buildEnv p)++(buildGraph p)++(buildQuiet p)
  
buildInput :: Parameters -> String
buildInput p =
 if (p # p_i_aterm) == "1"
  then 
   if (p # p_i_static) == "1"
    then
     if (p # p_i_bin) == "1"
      then
       if (p # p_i_basic_empty) == 0
        then " -input=aterm,static,bin,basic"
        else " -input=aterm,static,bin,empty"
      else 
       if (p # p_i_basic_empty) == 0
        then " -input=aterm,static,nobin,basic"
        else " -input=aterm,static,nobin,empty"
    else
     if (p # p_i_bin) == "1"
      then
       if (p # p_i_basic_empty) == 0
        then " -input=aterm,nostatic,bin,basic"
        else " -input=aterm,nostatic,bin,empty"
      else 
       if (p # p_i_basic_empty) == 0
        then " -input=aterm,nostatic,nobin,basic"
        else " -input=aterm,nostatic,nobin,empty"
  else 
   if (p # p_i_static) == "1"
    then
     if (p # p_i_bin) == "1"
      then
       if (p # p_i_basic_empty) == 0
        then " -input=casl,static,bin,basic"
        else " -input=casl,static,bin,empty"
      else 
       if (p # p_i_basic_empty) == 0
        then " -input=casl,static,nobin,basic"
        else " -input=casl,static,nobin,empty"
    else
     if (p # p_i_bin) == "1"
      then
       if (p # p_i_basic_empty) == 0
        then " -input=casl,nostatic,bin,basic"
        else " -input=casl,nostatic,bin,empty"
      else 
       if (p # p_i_basic_empty) == 0
        then " -input=casl,nostatic,nobin,basic"
        else " -input=casl,nostatic,nobin,empty"

buildOutput :: Parameters -> String
buildOutput p =
 if (p # p_o_tex) == "1"
  then
   if (p # p_o_binary) == "1"
    then
     if (p # p_o_e_encoding) /= 3
      then
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=tex,bin,env -spec=xml,aterm"
          else " -output=tex,bin,env -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=tex,bin,env -spec=noxml,aterm"
          else " -output=tex,bin,env -spec=noxml,noaterm"
      else 
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=tex,bin,noenv -spec=xml,aterm"
          else " -output=tex,bin,noenv -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=tex,bin,noenv -spec=noxml,aterm"
          else " -output=tex,bin,noenv -spec=noxml,noaterm"
    else 
     if (p # p_o_e_encoding) /= 3
      then
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=tex,nobin,env -spec=xml,aterm"
          else " -output=tex,nobin,env -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=tex,nobin,env -spec=noxml,aterm"
          else " -output=tex,nobin,env -spec=noxml,noaterm"
      else 
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=tex,nobin,noenv -spec=xml,aterm"
          else " -output=tex,nobin,noenv -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=tex,nobin,noenv -spec=noxml,aterm"
          else " -output=tex,nobin,noenv -spec=noxml,noaterm"
  else 
   if (p # p_o_binary) == "1"
    then
     if (p # p_o_e_encoding) /= 3
      then
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=notex,bin,env -spec=xml,aterm"
          else " -output=notex,bin,env -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=notex,bin,env -spec=noxml,aterm"
          else " -output=notex,bin,env -spec=noxml,noaterm"
      else 
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=notex,bin,noenv -spec=xml,aterm"
          else " -output=notex,bin,noenv -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=notex,bin,noenv -spec=noxml,aterm"
          else " -output=notex,bin,noenv -spec=noxml,noaterm"
    else 
     if (p # p_o_e_encoding) /= 3
      then
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=notex,nobin,env -spec=xml,aterm"
          else " -output=notex,nobin,env -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=notex,nobin,env -spec=noxml,aterm"
          else " -output=notex,nobin,env -spec=noxml,noaterm"
      else 
       if (p # p_o_xml) == "1"
        then
         if (p # p_o_aterm) == "1"
          then " -output=notex,nobin,noenv -spec=xml,aterm"
          else " -output=notex,nobin,noenv -spec=xml,noaterm"
        else 
         if (p # p_o_aterm) == "1"
          then " -output=notex,nobin,noenv -spec=noxml,aterm"
          else " -output=notex,nobin,noenv -spec=noxml,noaterm"

buildEnv :: Parameters -> String
buildEnv p = 
 case (p # p_o_e_encoding) of 
  0 -> " -env=CasEnv"++(buildEnvOutput (p # p_o_e_output))++(buildEnvOther (p # p_o_e_other))
  1 -> " -env=FCasEnv"++(buildEnvOutput (p # p_o_e_output))++(buildEnvOther (p # p_o_e_other))
  2 -> " -env=HCasEnv"++(buildEnvOutput (p # p_o_e_output))++(buildEnvOther (p # p_o_e_other))
  _ -> ""

buildEnvOutput :: Int -> String
buildEnvOutput i =
 case i of
  0 -> ",text"
  1 -> ",aterm"
  2 -> ",latex"
  3 -> ",xml"
  _ -> "" --who knows?

buildEnvOther :: Int -> String
buildEnvOther i =
 case i of
  0 -> ",SubPCFOL"
  1 -> ",SubCFOL"
  2 -> ",CFOL"
  3 -> ",SOL"
  4 -> ",CFOL1"
  5 -> ",SOL1"
  _ -> "" --who knows?

buildGraph :: Parameters -> String
buildGraph p =
  case (p # p_graph) of
   0 -> " -graph=davinci"
   1 -> " -graph=ps"
   2 -> " -graph=dot"
   _ -> ""

buildQuiet :: Parameters -> String
buildQuiet p =
 if (p # p_quiet) == "1"
  then ""
  else " -noquiet"




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
readFD :: Fd -> IO (Maybe (String));
readFD fd = 
 do 
  (s, bytes) <- catch (fdRead fd 1024) (\_ -> return ("", -1))
  case bytes of
   -1 -> do
          return (Nothing)
   _  -> do
          t <- readFD fd
	  case t of
	   Nothing -> return (Just s)
           Just ts -> return (Just (s++ts))


  