#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC 
{-# OPTIONS -#include "default_options.h" #-}
#endif /* NEW_GHC */

{- #########################################################################

   The WBFiles module is in charge of decoding information from the command
   line and making it available to the rest of the UniForM workbench.

   All UniForM options have names beginning with "--uni".  It is hoped
   that this won't be a problem for programs that use the UniForM workbench.
   However, if it is, the function
      setAlternateArgs
   should be called before any of the functions in the UniForM workbench,
   as this will prevent the program arguments being read by UniForM.

   The 
      --uni
   option prints a help message, as do other options beginning with
   --uni which are not understood.  

   The 
      --uni-parameters
   option prints the parameters at the given position on the command
   line.

   The
      --uni-version
   option prints the current version of uni.

   --uni-<option-name>:<option-value>
   or equivalently
   --uni-<option-name>=<option-value>

   All options can also be overridden by environment variables.
   The environment variable corresponding to <option-name> has the
   name UNI<OPTION-NAME>
   where <OPTION-NAME> is the capitalised name of the option.

   The default set of options are as follows:
   
   option-name   explanation
   
   wish          The filename of the wish program
   daVinci       The filename of daVinci
   cvs           The filename of cvs
   editor        A command to execute the text editor.  
                 This uses the CommandStringSub format, with defined
                 substitutions %F => where the file is to be found and
                 %N => what the user-visible name (for example, of the
                 buffer) should be.
   top           The directory in which UniForM is installed

   daVinciIcons  The directory containing daVinci icons
   backupDir     The directory to which the server programs should write
                 backup files
   workingDir    The directory used for temporary files.

   cvsRoot       The CVS root to be used for cvs
   server        The host name of the server
   port          The port on the server to connect to
   debug         Where Debug.debug messages should go

   simpleStore   Storage on disk of SimpleStore
   storeDir      Where Store stores its files

   toolTimeOut   Time-out waiting for responses from a tool when
                 it starts up and we are doing challenge-response
                 verification.

   The options wish, daVinci, cvs, daVinciIcons, top 
   should all be set automatically by the configure procedure.  
   The configure procedure constructs a variable DEFAULTOPTIONS
   and writes it into the file default_options.c.

   returns a string with exactly the same syntax as the command line
   (so a typical one might be
      "--uni-wish:/usr/bin/wish --uni-daVinci:/usr/bin/daVinci ... (and so on)
      )
   
   However one difference is that options which are not understood
   in the default_options string are simply ignored.
   
   ######################################################################### -}


module WBFiles (
   -- Functions for reading the results of initialising WBFiles.
   -- Values for which we provide defaults either here or in the
   -- configuration file can be accessed without Maybe.
   getWishPath, -- :: IO String 
      -- gets the path for wish
   getTixwishPath, -- :: IO String
      -- gets the path for tixwish
   getCVSPath, -- ditto cvs
   getDaVinciPath, -- ditto daVinci


   getToolTimeOut, -- :: IO Int
      -- gets tool time out.

   getTOP, -- ditto
   getEditorString, -- :: IO (Maybe String)
      -- returns editor string, if set.
   getPort, -- IO Int

   -- getBackupDir and getWorkingDir trim a right-file-separator
   -- from their arguments, if any.
   getBackupDir, -- IO String
   getWorkingDir, -- :: IO String

   -- getDebugFileName returns the name of the debug file.
   getDebugFileName, -- IO String
 
   -- values for which we don't are:
   getDaVinciIcons, -- :: IO (Maybe String)
   getCVSROOT, -- ditto
   getServer, -- ditto

   -- Store options.
   getSimpleStore, -- :: IO String
   getStoreDir, -- :: IO String

   -- Access to other options.
   getArgString, -- :: String -> IO (Maybe String)
   getArgBool, -- :: String -> IO (Maybe Bool)
   getArgInt, -- :: String -> IO (Maybe Int)
   

   -- getWB* are provided for compatibility only.  DO NOT USE.
   getWBToolFilePath, -- :: String-> IO String
      -- gets path for tools, assuming it's TOP/database/bin/++ argument.
   getWBImageFilePath, -- :: String -> IO String
      -- gets path for images, assuming its TOP/database/images/++ argument.

   -- Functions for initialising WBFiles.  If they detect an error
   -- in the parse, they immediately do System.exitWith (ExitFailure 4).
   -- If the --uni option is used, they do System.exitWith (ExitSuccess)
   -- (after displaying a help message).
   -- If none of these functions are used, the arguments are parsed when
   -- we first try to access them, with the same effect as parseArguments
   -- except that we don't exit if there's a problem.
   -- 
   parseArguments, -- :: IO ()
       -- equivalent to parseTheseArguments usualProgramArguments.
       -- parseArguments is done by default
   parseArgumentsRequiring, -- :: [String] -> IO ()
       -- equivalent to parseTheseArgumentsRequiring usualProgramArguments.

   ArgType(..), -- represents type arguments can have.
   ArgValue(..), -- represents values arguments can have.

   ProgramArgument(..), -- data corresponding to a single sort of argument.
   
   usualProgramArguments, 
      -- :: [ProgramArgument]
      -- corresponds to the usual program arguments.

   parseTheseArguments, -- :: [ProgramArgument] -> IO () 
   -- parseTheseArguments args = parseTheseArgumentsRequiring args []

   parseTheseArgumentsRequiring, -- :: [ProgramArgument] -> [String] -> IO ()
   -- parseTheseArgumentsRequiring
   -- parses the arguments, using the supplied list of allowed arguments.
   -- It is an error if any of the options with names in the second argument
   -- are not defined.
   
   setAlternateArgs, -- :: [String] -> IO ()
   -- specify the given strings as arguments to be used by the parse 
   -- functions.
   
   ) where

import Char
import IO
import List
import Monad
import qualified System
import System(exitWith,ExitCode(..))

import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO)
import qualified Addr
import qualified CString

import FileNames

#include "config.h"

------------------------------------------------------------------------
-- Specific access functions.
------------------------------------------------------------------------

valOf :: IO (Maybe a) -> IO a
valOf action =
   do
      valueOpt <- action
      case valueOpt of
         Just a -> return a
         Nothing -> error ("An option is surprisingly unset")

getTixwishPath :: IO String
getTixwishPath = valOf (getArgString "tixwish")

getWishPath :: IO String 
getWishPath = valOf (getArgString "wish")

getCVSPath :: IO String
getCVSPath = valOf (getArgString "cvs")

getEditorString :: IO (Maybe String)
getEditorString = getArgString "editor" 

getDaVinciPath :: IO String
getDaVinciPath = valOf (getArgString "daVinci")

getToolTimeOut :: IO Int
getToolTimeOut = valOf (getArgInt "toolTimeOut")

getTOP :: IO String
getTOP = valOf (getArgString "top")

getPort :: IO Int
getPort = valOf (getArgInt "port")

getBackupDir :: IO String
getBackupDir = 
   do
      backupDir' <- valOf (getArgString "backupDir")
      return (trimDir backupDir')

getWorkingDir :: IO String
getWorkingDir = 
   do
      workingDir' <- valOf (getArgString "workingDir")
      return (trimDir workingDir')

getDebugFileName :: IO String
getDebugFileName = valOf (getArgString "debug")

getSimpleStore :: IO String
getSimpleStore = valOf (getArgString "simpleStore")

getStoreDir :: IO String
getStoreDir = valOf (getArgString "storeDir")

getDaVinciIcons :: IO (Maybe String)
getDaVinciIcons = getArgString "daVinciIcons"

getCVSROOT :: IO (Maybe String)
getCVSROOT = getArgString "cvsRoot"

getServer :: IO (Maybe String)
getServer = getArgString "server"

getWBToolFilePath :: String-> IO String
getWBToolFilePath tool =
   do
      top <- getTOP
      return ((trimDir top)++"/database/bin/"++tool)

getWBImageFilePath :: String -> IO String
getWBImageFilePath image =
   do
      top <- getTOP
      return ((trimDir top)++"/database/images/"++image)

------------------------------------------------------------------------
-- ProgramArgument and usualProgramArguments.
------------------------------------------------------------------------

data ProgramArgument = ProgramArgument {
   optionName :: String, -- the option name
   optionHelp :: String, -- Help text displayed by --uni option.
   defaultVal :: Maybe ArgValue, -- default value
   argType :: ArgType
   }

usualProgramArguments :: [ProgramArgument]
usualProgramArguments = [
   ProgramArgument{
      optionName = "wish",
      optionHelp = "path to the wish program",
      defaultVal = Just (StringValue "/usr/bin/wish"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "daVinci",
      optionHelp = "path to the daVinci program",
      defaultVal = Just (StringValue "/usr/bin/daVinci"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "daVinciIcons",
      optionHelp = "directory containing daVinci icons",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "toolTimeOut",
      optionHelp = "time-out when tools start up in milliseconds",
      defaultVal = Just (IntValue 10000),
      argType = INT
      },
   ProgramArgument{
      optionName = "cvs",
      optionHelp = "path to the cvs program",
      defaultVal = Just (StringValue "/usr/bin/cvs"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "editor",
      optionHelp = "text editor cmd; %F => filename; %N => user-visible name",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "cvsRoot",
      optionHelp = "CVSROOT for CVS",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "top",
      optionHelp = "path where UniForM was installed",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "backupDir",
      optionHelp = "directory where servers backup files",
      defaultVal = Just (StringValue "."),
      argType = STRING
      },
   ProgramArgument{
      optionName = "simpleStore",
      optionHelp = "where server store goes",
      defaultVal = Just (StringValue "serverStore"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "storeDir",
      optionHelp = "where server store directory goes",
      defaultVal = Just (StringValue "storeDir"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "workingDir",
      optionHelp = "directory used for temporary files",
      defaultVal = Just (StringValue "/tmp"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "server",
      optionHelp = "machine where the server runs",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "port",
      optionHelp = "port for the server",
      defaultVal = Just (IntValue 11393),
      argType = INT
      },
   ProgramArgument{
      optionName = "debug",
      optionHelp = "file for debug output",
      defaultVal = Just (StringValue "/tmp/uniform.DEBUG"),
      argType = STRING
      }
   ]


------------------------------------------------------------------------
-- Argument Types
------------------------------------------------------------------------

data ArgType = STRING | INT | BOOL

showArgType :: ArgType -> String
showArgType STRING = "string"
showArgType INT = "int"
showArgType BOOL = "bool"

data ArgValue = StringValue String | IntValue Int | BoolValue Bool

parseArgValue :: ArgType -> String -> Maybe ArgValue
parseArgValue STRING str = Just (StringValue str)
parseArgValue INT str =
   case readsPrec 0 str of
      [(val,"")] -> Just (IntValue val)
      _ -> Nothing
parseArgValue BOOL str =
   let
      true = Just (BoolValue True)
      false = Just (BoolValue False)
   in
      case str of
         "" -> true
         "True" -> true
         "False" -> false
         "+" -> true
         "-" -> false
         "yes" -> true
         "no" -> false
         _ -> Nothing

showArgValue (StringValue str) = str
showArgValue (IntValue i) = show i
showArgValue (BoolValue b) = if b then "+" else "-"

------------------------------------------------------------------------
-- Parsed Arguments
------------------------------------------------------------------------

newtype ParsedArguments = 
   ParsedArguments (MVar (Maybe (FiniteMap String ArgValue)))

makeParsedArguments :: IO ParsedArguments
makeParsedArguments =
   do
      mVar <- newMVar Nothing
      return (ParsedArguments mVar)
{-# NOINLINE makeParsedArguments #-}
-- the NOINLINE should, we hope, mean that there is only one copy of
-- the parsedArguments mVar.

parsedArguments :: ParsedArguments
-- the unique set of parsed arguments
parsedArguments = IOExts.unsafePerformIO makeParsedArguments
{-# NOINLINE parsedArguments #-}

getArgValue :: String -> IO (Maybe ArgValue)
getArgValue optionName =
   do
      map <- forceParseArguments 
      return (lookupFM map optionName)

mismatch :: String -> a
mismatch optionName = 
   error ("WBFiles.mismatch - type mismatch for "++optionName)
   -- If this happens, it means a bug in this file or else
   -- a default value for a program argument does not have the right type,
   -- or an attempt to use a getArg* function for an option with the wrong
   -- type.
{-# NOINLINE mismatch #-}

getArgString :: String -> IO (Maybe String)
getArgString optionName =
   do
      valOpt <- getArgValue optionName
      case valOpt of
         Just (StringValue str) -> return (Just str)
         Just _ -> mismatch optionName
         Nothing -> return Nothing

getArgInt :: String -> IO (Maybe Int)
getArgInt optionName =
   do
      valOpt <- getArgValue optionName
      case valOpt of
         Just (IntValue i) -> return (Just i)
         Just _ -> mismatch optionName
         Nothing -> return Nothing
         

getArgBool :: String -> IO (Maybe Bool)
getArgBool optionName =
   do
      valOpt <- getArgValue optionName
      case valOpt of
         Just (BoolValue b) -> return (Just b)
         Just _ -> mismatch optionName
         Nothing -> return Nothing
         

-- forceParseArguments is used to force a parse of the arguments
-- when no parse function has been called before.
forceParseArguments :: IO (FiniteMap String ArgValue)
forceParseArguments =
   do
      let ParsedArguments mVar = parsedArguments
      mapOpt <- takeMVar mVar
      case mapOpt of
         Nothing ->
            do  
               (exitCode,newMap) <- 
                  parseTheseArgumentsRequiring' usualProgramArguments []
               putMVar mVar (Just newMap)
               return newMap
         Just map ->
            do
               putMVar mVar (Just map)
               return map
                             
------------------------------------------------------------------------
-- setAlternateArgs
------------------------------------------------------------------------

alternateArgs :: MVar [String]

newAlternateArgs :: IO (MVar [String])
newAlternateArgs = newEmptyMVar
{-# NOINLINE newAlternateArgs #-}

alternateArgs = IOExts.unsafePerformIO newAlternateArgs

setAlternateArgs :: [String] -> IO ()
setAlternateArgs newArgs = 
   do
      isEmpty <- isEmptyMVar alternateArgs
      if isEmpty
         then
            putMVar alternateArgs newArgs
         else
            error "setAlternateArgs called twice or after getArgs"

getArgs :: IO [String]
getArgs =
   do
      isEmpty <- isEmptyMVar alternateArgs
      args <- if isEmpty
         then
            System.getArgs
         else
            takeMVar alternateArgs
      putMVar alternateArgs args
      return args

------------------------------------------------------------------------
-- Parsing Arguments
------------------------------------------------------------------------

parseArguments :: IO ()
parseArguments = parseTheseArguments usualProgramArguments

parseArgumentsRequiring :: [String] -> IO ()
parseArgumentsRequiring required = 
   parseTheseArgumentsRequiring usualProgramArguments required

parseTheseArguments :: [ProgramArgument] -> IO ()
parseTheseArguments arguments = parseTheseArgumentsRequiring arguments []

parseTheseArgumentsRequiring :: [ProgramArgument] -> [String] -> IO ()
parseTheseArgumentsRequiring arguments required =
   do
      let ParsedArguments mVar = parsedArguments
      mapOpt <- takeMVar mVar
      case mapOpt of
         Just _ ->
            do
               putMVar mVar mapOpt
               printToErr
                  ("WBFiles.parseTheseArgumentsRequiring: " ++
                     "attempt to parse arguments too late")
         Nothing ->
            do
               (result,newMap) <- 
                  parseTheseArgumentsRequiring' arguments required
               putMVar mVar (Just newMap)
               case result of
                  Nothing -> return ()
                  Just exitCode -> exitWith exitCode


type ParseState = (Maybe ExitCode,FiniteMap String ArgValue)

parseTheseArgumentsRequiring' :: [ProgramArgument] -> [String] -> 
  IO ParseState
-- is the most general argument parsing function, in terms of which
-- all the others are defined. 
-- It returns a map representing the parsed arguments, plus an exit
-- code if an exit is indicated.
parseTheseArgumentsRequiring' arguments required =
   do
      let
         initialMap =
            foldl
               (\ map argument ->
                  case (defaultVal argument) of
                     Nothing -> map
                     Just value -> addToFM map (optionName argument) value
                  )
               emptyFM
               arguments

         (initial :: ParseState) = (Nothing,initialMap)

#ifndef NEW_GHC
      afterDefault <- foldM (handleParameter False) initial defaultOptions
#else /* NEW_GHC */
      defaultOptionsStr <- CString.peekCString defaultOptions
      afterDefault <- foldM (handleParameter False) initial 
         (words defaultOptionsStr)
#endif /* NEW_GHC */

      parameters <- getArgs

      afterParms <- foldM (handleParameter True) afterDefault parameters

      afterEnvs <- foldM handleEnv afterParms arguments
 
      foldM checkReq afterEnvs required
   where
      handleParameter :: Bool -> ParseState -> String -> IO ParseState
      -- handles a single command line parameter.  If the Bool is true
      -- it modifies the exit code accordingly.
      handleParameter noticeErrors prev@(prevExit,prevMap) parameter =
         let
            newExit exitCode = upgradeError noticeErrors exitCode prevExit
            cantParse =
               do
                  printToErr ("Can't parse "++parameter)
                  displayHelp
                  return (newExit (ExitFailure 4),prevMap)
         in
            case parameter of
               "--uni" -> 
                  do
                     displayHelp
                     return (newExit ExitSuccess,prevMap)
               "--uni-version" -> 
                     do
                        printToErr ("uni's version is "++UNIVERSION)
                        return (newExit ExitSuccess,prevMap)
               "--uni-parameters" ->
                  do
                     displayState prevMap
                     return (newExit ExitSuccess,prevMap) 
               '-':'-':'u':'n':'i':'-':setParm ->
                  case splitSetPart setParm of
                     Nothing -> cantParse
                     Just (option,value) ->
                        case find (\ arg -> optionName arg == option) 
                              arguments of 
                           Nothing -> 
                              do
                                 if noticeErrors 
                                    then
                                       do
                                          displayHelp
                                          printToErr ("Option '"++option++
                                             "' not recognised")
                                    else
                                       return ()
                                 return (newExit (ExitFailure 4),prevMap)
                           Just arg -> 
                              tryToAddValue (argType arg) option value prev
               '-':'-':'u':'n':'i':_ -> cantParse
               _ -> return prev

      tryToAddValue :: ArgType -> String -> String -> ParseState -> 
         IO ParseState
      tryToAddValue argType option value prev@(prevExit,prevMap) =
         case parseArgValue argType value of
            Nothing ->
               do
                  printToErr("For --uni-"++ option ++ ", "++(show value)++
                     " isn't "++ (showArgType argType))
                  return 
                     (upgradeError True (ExitFailure 4) prevExit,prevMap)
                     -- we always take notice of this error, since it
                     -- shouldn't occur in the default list either.
            Just argValue ->
               return (prevExit,addToFM prevMap option argValue)
                                 
      splitSetPart :: String -> Maybe (String,String)
      -- splitSetPart splits the string at its first : or = and returns
      -- the result
      splitSetPart "" = Nothing
      splitSetPart (':':rest) = Just ("",rest)
      splitSetPart ('=':rest) = Just ("",rest)
      splitSetPart (first:rest) =
         case splitSetPart rest of
            Nothing -> Nothing
            Just (left,right) -> Just (first:left,right)

      displayHelp :: IO ()
      -- display a help message
      displayHelp =
         do
            printToErr "Command-line options:"
            printToErr "--uni displays this message"
            printToErr "--uni-version displays the current version"
            printToErr "--uni-parameters displays option settings"
            sequence_
               (map
                  (\ (ProgramArgument{optionName = optionName,
                     optionHelp = optionHelp,argType = argType}) ->
                     printToErr (
                        "--uni-"++optionName++"=["++showArgType argType ++
                        "] sets "++optionHelp
                        )
                     )
                  arguments
                  )

      displayState :: FiniteMap String ArgValue -> IO ()
      -- displays the current options
      displayState fmap =
         do
            let optionValues = fmToList fmap
            printToErr "Parameter settings:"
            sequence_ 
               (map
                  (\ (option,argValue) ->
                     printToErr ("--uni-"++option++"="++
                        (showArgValue argValue))
                     )
                  optionValues
                  )

      handleEnv :: ParseState -> ProgramArgument -> IO ParseState
      -- look up the environment variable for the program argument and
      -- adjust state appropriately
      handleEnv prev@(prevExit,prevMap) arg =
         do
            let
               option = optionName arg 
               envVar = "UNI"++(map toUpper option)
            valueOpt <- try (System.getEnv envVar)
            case valueOpt of
               Left error -> return prev
               Right newValue -> 
                  tryToAddValue (argType arg) option newValue prev

      checkReq :: ParseState -> String -> IO ParseState
      -- check that the provided option value is set
      checkReq prev@(prevExit,prevMap) option =
         case lookupFM prevMap option of
            Just _ -> return prev
            Nothing -> 
               do
                  printToErr ("Option "++option++" is not set.")
                  return (upgradeError True (ExitFailure 4) prevExit,prevMap)

      upgradeError :: Bool -> ExitCode -> Maybe ExitCode -> Maybe ExitCode
      -- takes notice of an error, if the first argument is set.
      upgradeError False _ soFar = soFar
      upgradeError True exitCode Nothing = Just exitCode
      upgradeError True exitCode (Just ExitSuccess) = Just exitCode
      upgradeError True ExitSuccess (Just exitCode) = Just exitCode
      upgradeError True (ExitFailure level1) (Just (ExitFailure level2)) =
         Just (ExitFailure (max level1 level2))

#ifndef NEW_GHC
defaultOptions :: [String]
defaultOptions = words (CString.unpackCString addr_defaultOptions)
#else /* NEW_GHC */
foreign import ccall  "default_options.h & default_options" defaultOptions :: CString.CString
#endif /* NEW_GHC */

#ifndef NEW_GHC
foreign label "default_options" addr_defaultOptions :: Addr.Addr

#endif /* NEW_GHC */
------------------------------------------------------------------------
-- Printing to stderr.
------------------------------------------------------------------------

printToErr :: String -> IO ()
printToErr message =
   do
      hPutStrLn stderr message

