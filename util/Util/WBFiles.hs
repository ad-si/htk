{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

-- |
-- Description : Option processing
--
-- The WBFiles module is in charge of decoding information from the command
-- line and making it available to the rest of the UniForM workbench.
--
-- All UniForM options have names beginning with "--uni".  It is hoped
-- that this won't be a problem for programs that use the UniForM workbench.
-- However, if it is, the function
--    setAlternateArgs
-- should be called before any of the functions in the UniForM workbench,
-- as this will prevent the program arguments being read by UniForM.
--
-- The
-- @
--    --uni
-- @
-- option prints a help message, as do other options beginning with
-- --uni which are not understood.
--
-- The
-- @
--    --uni-parameters
-- @
-- option prints the parameters at the given position on the command
-- line.
--
-- The
-- @
--    --uni-version
-- @
-- option prints the current version of uni.
--
-- @
-- --uni-<option-name>:<option-value>
-- @
-- or equivalently
-- @
-- --uni-<option-name>=<option-value>
-- @
--
-- All options can also be overridden by environment variables.
-- The environment variable corresponding to <option-name> has the
-- name @UNI<OPTION-NAME>@
-- where @<OPTION-NAME>@ is the capitalised name of the option.
--
-- The default set of options are as follows:
--
-- option-name   explanation
--
-- wish          The filename of the wish program
-- daVinci       The filename of daVinci
-- gnuclient     The filename of gnuclient
-- editor        A command to execute the text editor.
--               This uses the CommandStringSub format, with defined
--               substitutions %F => where the file is to be found and
--               %N => what the user-visible name (for example, of the
--               buffer) should be.
-- top           The directory in which UniForM is installed
--
-- daVinciIcons  The directory containing daVinci icons
--
-- workingDir    The directory used for temporary files.
--
-- server        The host name of the server
-- user          The user-id to use connecting to the server
-- password      The password to use connecting to the server
-- port          The port on the server to connect to
-- xmlPort       The port for the XML server (which has a different default)
--
-- debug         Where Debug.debug messages should go
--
-- serverDir     Where Server stores its files
-- serverId      The unique identifier of the server.
--               Since this really does have to be globally unique,
--               it is by default constructed from a combination
--               of the machine's hostname and the server port.
--               You had better not change it unless you know what
--               you are doing.
--
-- MMiSSDTD      Location of DTD file for MMiSS.
--
-- hosts         Location of hosts file.
--
-- toolTimeOut   Time-out waiting for responses from a tool when
--               it starts up and we are doing challenge-response
--               verification.
-- windowsTick   (Windows only) time in microseconds we wait between
--               polling Wish.
--
-- The options wish, daVinci, daVinciIcons, top
-- should all be set automatically by the configure procedure.
-- The configure procedure constructs a variable DEFAULTOPTIONS
-- and writes it into the file default_options.c.
--
-- returns a string with exactly the same syntax as the command line
-- so a typical one might be
--    @
--    --uni-wish:/usr/bin/wish --uni-daVinci:/usr/bin/daVinci
--    @
--    ... (and so on)
--
-- However one difference is that options which are not understood
-- in the default_options string are simply ignored.
module Util.WBFiles (
   -- Functions for reading the results of initialising WBFiles.
   -- Values for which we provide defaults either here or in the
   -- configuration file can be accessed without Maybe.
   getWishPath, -- :: IO String
      -- gets the path for wish
   getDaVinciPath,
      -- ditto daVinci
   getGnuClientPath,
      -- ditto gnuclient.

   getToolTimeOut, -- :: IO Int
      -- gets tool time out.
   getTOP, --  :: IO String
      -- Get the location of the top directory.
   getTOPPath,
      -- :: [String] -> IO String
      -- Get a path within the top directory.
   getEditorString, -- :: IO (Maybe String)
      -- returns editor string, if set.
   getMMiSSDTD, -- :: IO (Maybe String)
      -- returns location of MMiSSDTD, if set.
   getMMiSSAPIDTD, -- :: IO (Maybe String)
      -- returns location of DTD for API requests, if set.
      -- (does not correspond to an option at present, we get it from TOP)

   getHosts, -- :: IO String
      -- returns location of hosts file.

   getPort, -- IO Int

   getXMLPort, -- IO Int

   getCouplingPort, -- IO Int

   -- getWorkingDir trims a right-file-separator from its argument, if any.
   getWorkingDir, -- :: IO String

   getCouplingDir, -- :: IO String

   -- getDebugFileName returns the name of the debug file.
   getDebugFileName, -- IO String

   -- values for which we don't are:
   getDaVinciIcons, -- :: IO (Maybe String)
   getServer, -- ditto
   getUser, -- ditto
   getPassword, -- ditto

   -- Store options.
   getServerFile, -- :: String -> IO String
      -- Get a file for the use of the server.
   getServerDir, --  :: IO String
      -- Get the server's private directory.
   getServerId, -- :: IO (Maybe String)
      -- Return a (globally unique) id for this server.

   -- Access to other options.
   getArgString, -- :: String -> IO (Maybe String)
   getArgBool, -- :: String -> IO (Maybe Bool)
   getArgInt, -- :: String -> IO (Maybe Int)

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

   setAlternateArgs -- :: [String] -> IO ()
   -- specify the given strings as arguments to be used by the parse
   -- functions.

   ) where

import Data.Char
import Util.CompileFlags
import System.IO
import Data.List
import Control.Monad
import qualified System.Environment as System
import System.Exit(exitWith,ExitCode(..))

import qualified Control.Exception as Exception
import Control.Concurrent
import qualified Data.Map as Map
import System.IO.Unsafe
import Foreign.C.String

import Util.FileNames

------------------------------------------------------------------------
-- Specific access functions.
------------------------------------------------------------------------

valOf :: String -> IO (Maybe a) -> IO a
valOf optionName action =
   do
      valueOpt <- action
      case valueOpt of
         Just a -> return a
         Nothing ->
            error ("option --uni-" ++ optionName ++ " is surprisingly unset")

getWishPath :: IO String
getWishPath = valOf "wish" (getArgString "wish")

getEditorString :: IO (Maybe String)
getEditorString = getArgString "editor"

getMMiSSDTD :: IO (Maybe String)
getMMiSSDTD =
   do
      mmissDTDOpt <- getArgString "MMiSSDTD"
      case mmissDTDOpt of
         Just mmissDTD -> return mmissDTDOpt
         Nothing ->
            do
               path <- getTOPPath ["mmiss","MMiSS.dtd"]
               return (Just path)

getMMiSSAPIDTD :: IO (Maybe String)
getMMiSSAPIDTD =
   do
      path <- getTOPPath ["mmiss","api","MMiSSRequest.dtd"]
      return (Just path)

      -- returns location of DTD for API requests, if set.

getHosts :: IO String
getHosts =
   do
      hostsOpt <- getArgString "Hosts"
      case hostsOpt of
         Just hosts -> return hosts
         Nothing ->
            getTOPPath ["server","Hosts.xml"]


getDaVinciPath :: IO String
getDaVinciPath = valOf "daVinci" (getArgString "daVinci")

getGnuClientPath :: IO String
getGnuClientPath = valOf "gnuclient" (getArgString "gnuclient")

getToolTimeOut :: IO Int
getToolTimeOut = valOf "toolTimeOut" (getArgInt "toolTimeOut")

getTOP :: IO String
getTOP = valOf "top" (getArgString "top")

-- | Get a path within the top directory.
getTOPPath :: [String] -> IO String
getTOPPath names =
   do
      top <- getTOP
      return (unbreakName (trimDir top:names))

getPort :: IO Int
getPort = valOf "port" (getArgInt "port")

getXMLPort :: IO Int
getXMLPort = valOf "xmlPort" (getArgInt "xmlPort")

getWorkingDir :: IO String
getWorkingDir =
   do
      workingDir' <- valOf "workingDir" (getArgString "workingDir")
      return (trimDir workingDir')

getDebugFileName :: IO String
getDebugFileName = valOf "debug" (getArgString "debug")

getServerFile :: String -> IO String
getServerFile innerName =
   do
      serverDir <- getServerDir
      return (combineNames (trimDir serverDir) innerName)

getServerDir :: IO String
getServerDir =
   do
      serverDirOpt <- getArgString "serverDir"
      case serverDirOpt of
         Nothing ->
            error (
               "UNISERVERDIR environment variable or --uni-serverDir"
               ++ " must be set for server programs")
         Just serverDir -> return serverDir

getServerId :: IO (Maybe String)
getServerId = getArgString "serverId"


getDaVinciIcons :: IO (Maybe String)
getDaVinciIcons = getArgString "daVinciIcons"

getServer :: IO (Maybe String)
getServer = getArgString "server"

getUser :: IO (Maybe String)
getUser = getArgString "user"

getPassword :: IO (Maybe String)
getPassword = getArgString "password"

getCouplingPort :: IO Int
getCouplingPort = valOf "couplingPort" (getArgInt "couplingPort")

getCouplingDir ::  IO String
getCouplingDir = valOf "couplingDir" (getArgString "couplingDir")


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
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "daVinciIcons",
      optionHelp = "directory containing daVinci icons",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "gnuclient",
      optionHelp = "path to the gnuclient program",
      defaultVal = Just (StringValue "gnuclient"),
      argType = STRING
      },
   ProgramArgument{
      optionName = "toolTimeOut",
      optionHelp = "time-out when tools start up in milliseconds",
      defaultVal = Just (IntValue 10000),
      argType = INT
      },
   ProgramArgument{
      optionName = "windowsTick",
      optionHelp = "interval in microseconds for polling wish (Windows only).",
      defaultVal = Just (IntValue 10000),
      argType = INT
      },
   ProgramArgument{
      optionName = "editor",
      optionHelp = "text editor cmd; %F => filename; %N => user-visible name",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      -- We make getMMiSSDTD return a default of TOP/mmiss/MMiSS.dtd if
      -- nothing is set.
      optionName = "MMiSSDTD",
      optionHelp = "Filename for MMiSS's DTD",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      -- We make getHosts return a default of TOP/server/Hosts.xml if
      -- Nothing is set.
      optionName = "Hosts",
      optionHelp = "File containing list of hosts",
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
      optionName = "serverDir",
      optionHelp = "where server stores its files",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "serverId",
      optionHelp = "globally unique server identifier (EXPERTS ONLY)",
      defaultVal = Nothing,
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
      optionName = "user",
      optionHelp = "Your identifier on the server",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "password",
      optionHelp = "Your password on the server",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "port",
      optionHelp = "port for the server",
      defaultVal = Just (IntValue defaultPort),
      argType = INT
      },
   ProgramArgument{
      optionName = "xmlPort",
      optionHelp = "port for the MMiSS-XML server",
      defaultVal = Just (IntValue defaultXMLPort),
      argType = INT
      },
   ProgramArgument{
      optionName = "couplingPort",
      optionHelp = "port for the coupling server",
      defaultVal = Just (IntValue defaultCouplingPort),
      argType = INT
      },
   ProgramArgument{
      optionName = "couplingDir",
      optionHelp = "directory where the coupling server finds the working copy of foreign repository",
      defaultVal = Nothing,
      argType = STRING
      },
   ProgramArgument{
      optionName = "debug",
      optionHelp = "file for debug output",
      defaultVal = Just (StringValue "/tmp/uniform.DEBUG"),
      argType = STRING
      }
   ]

defaultPort :: Int
defaultPort = 11393


defaultXMLPort :: Int
defaultXMLPort = 11396

defaultCouplingPort :: Int
defaultCouplingPort = 11391

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

showArgValue :: ArgValue -> String
showArgValue (StringValue str) = str
showArgValue (IntValue i) = show i
showArgValue (BoolValue b) = if b then "+" else "-"

------------------------------------------------------------------------
-- Parsed Arguments
------------------------------------------------------------------------

newtype ParsedArguments =
   ParsedArguments (MVar (Maybe (Map.Map String ArgValue)))

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
parsedArguments = unsafePerformIO makeParsedArguments
{-# NOINLINE parsedArguments #-}

getArgValue :: String -> IO (Maybe ArgValue)
getArgValue optionName =
   do
      map <- forceParseArguments
      return (Map.lookup optionName map)

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
forceParseArguments :: IO (Map.Map String ArgValue)
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

alternateArgs = unsafePerformIO newAlternateArgs

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


type ParseState = (Maybe ExitCode,Map.Map String ArgValue)

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
                     Just value -> Map.insert (optionName argument) value map
                  )
               Map.empty
               arguments

         initial = (Nothing, initialMap) :: ParseState

      defaultOptionsStr <- peekCString defaultOptions
      afterDefault <- foldM (handleParameter False) initial
         (words defaultOptionsStr)

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
                        printToErr ("uni's version is "++uniVersion)
                        -- The MMiSS installer relies on the exact text of
                        -- this message.
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
               return (prevExit,Map.insert option argValue prevMap)

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

      displayState :: Map.Map String ArgValue -> IO ()
      -- displays the current options
      displayState fmap =
         do
            let optionValues = Map.toList fmap
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
            valueOpt <- Exception.try (System.getEnv envVar)
            case valueOpt of
               Left (_ :: Exception.IOException) -> return prev
               Right newValue ->
                  tryToAddValue (argType arg) option newValue prev

      checkReq :: ParseState -> String -> IO ParseState
      -- check that the provided option value is set
      checkReq prev@(prevExit,prevMap) option =
         case Map.lookup option prevMap of
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

foreign import ccall  "default_options.h & default_options"
   defaultOptions :: CString

------------------------------------------------------------------------
-- Printing to stderr.
------------------------------------------------------------------------

printToErr :: String -> IO ()
printToErr message =
   do
      hPutStrLn stderr message
