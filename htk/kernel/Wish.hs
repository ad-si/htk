module Wish (

  wish,

  evalTclScript,
  execTclScript,
  execCmd,
  evalCmd,
  escape,
  delimitString,
  Wish(..),
  TclCmd,
  TclScript,
  TclResponse(..),
  TclMessageType(..),
  BindTag,
  bindTagS,
  succBindTag,
  WishEvent(..),
  WishEventModifier(..),
  WishEventType(..),
  mkBoundCmdArg,
  KeySym(..),
  BNo(..),
  CallBackId(..),
  showP,

  tixAvailable, -- :: Bool.  True if we are using tixwish.
) where

import Maybe
import Char
import List(find)

import IOExts
import Posix
import Concurrent
import ByteArray
import CString

import Debug
import Object
import Computation
import WBFiles

import Events
import Channels
import GuardedEvents
import EqGuard
import Destructible

import FdRead
import ChildProcess

import ReferenceVariables
import EventInfo

-- -----------------------------------------------------------------------
-- basic execution of Tcl commands
-- -----------------------------------------------------------------------

evalTclScript :: TclScript -> IO String
evalTclScript cmds =
  do
    let cmd = concat (map (\s -> s ++ ";") cmds)
    res <- execSingle cmd
    return res
  where execSingle :: TclCmd -> IO String
        execSingle cmd = do
                           resp <- evalCmd cmd
                           case resp of
                             ER str -> error str
                             OK str -> return str

execTclScript :: TclScript -> IO ()
execTclScript cmds = evalTclScript cmds >> done

execCmd :: TclCmd -> IO ()
execCmd cmd =
  do
    resp <- evalCmd cmd
    case resp of
      ER str -> error str
      _ -> done

evalCmd :: TclCmd -> IO TclResponse
evalCmd cmd =
   do
      let
         str = "evS "++ escape cmd ++ "\n"
         -- We go to some trouble to pack the string first,
         -- as this has the side-effect of meaning it will
         -- be fully evaluated, so we are locked for minimum time. 
         packed = packString str
         len = length str
         rWish = readWish wish
         wWishMVar = writeWishMVar wish
      wWish <- rWish `seq` packed `seq` len `seq` 
         takeMVar (writeWishMVar wish)
      wWish packed len
      result <- sync(
            toEvent (rWish |> Eq OKType) >>>=
               (\ (_,okString) -> return (OK okString))
         +> toEvent (rWish |> Eq ERType) >>>=
               (\ (_,erString) -> return (ER erString))
         )
      putMVar wWishMVar wWish
      return result


-- ------------------------------------------------------------------
-- escaping strings 
-- ------------------------------------------------------------------

escape :: String -> String
escape = delimitString . escapeString

-- escapeString quotes the special characters inside String.
escapeString :: String -> String
escapeString = concat . (map quoteChar)

-- delimitString places quotes around a String, if it contains
-- spaces, making it possible to use it as a single argument.
delimitString :: String -> String
delimitString "" = "\"\""
delimitString str =
   if isJust (find isSpace str)
   then
      '\"':(str++"\"")
   else
      str

-- quoteChar quotes characters special to Tcl, but not %.
quoteChar :: Char -> String
quoteChar ch =
   case ch of
      '\\' -> "\\\\"
      '\"' -> "\\\""
      '\n' -> "\\n"
      '{' -> "\\{"
      '}' -> "\\}"
      '$' -> "\\$"
      '[' -> "\\["
      ']' -> "\\]"
      ';' -> "\\;"
      other -> 
         if isPrint ch
            then
               [ch]
            else
               let
                  nchar = ord ch
               in
                  if (nchar<0 || nchar >=256)
                     then
                        error "TclSyntax: bad char"
                     else
                        let
                           (hi,lo) = nchar `divMod` 16
                        in
                           ['\\','x',intToDigit hi,intToDigit lo]

-- -----------------------------------------------------------------------
-- wish datatypes
-- -----------------------------------------------------------------------

data Wish = Wish {
   commands :: Channel TclScript,
   -- commands contains the commands to execute.  Since all commands
   -- are of necessity single-threaded, we provide a single answer queue
   -- responses.  (But this may change!) 

   responses :: Channel TclResponse,

   eventQueue :: EqGuardedChannel BindTag EventInfo,
   -- Wish puts events here, parameterised by the 
   -- widget tag, which for us is always a widget id.
   -- The events will be taken off by the event dispatcher.

   coQueue :: EqGuardedChannel CallBackId (),
   -- CO events, produced by the relay command, go here.
   -- (These are used for Widgets with actions attached,
   -- EG for buttons with "Click me" on them.),

--   callBackIds :: MVar CallBackId,

   bindTags :: MVar BindTag,

   readWish :: 
      GuardedEvent (EqMatch TclMessageType) (TclMessageType,String),
      -- Wish output sorted by prefix. 

   writeWishMVar :: MVar(ByteArray Int -> Int -> IO ()), 
      -- Command to execute a Wish command.  This is an MVar
      -- and locked by emptying it.

   destroyWish :: IO () -- Command to destroy this Wish instance.
   }

type TclCmd = String
type TclScript = [TclCmd]

data TclResponse = OK String | ER String

data TclMessageType = OKType | ERType | COType | EVType deriving (Eq,Ord,Show)


-- ----------------------------------------------------------------
-- wish instances
-- ----------------------------------------------------------------

instance Destroyable Wish where
   destroy wish = destroyWish wish


-- ----------------------------------------------------------------
-- running the wish
-- ----------------------------------------------------------------

wish :: Wish
wish = IOExts.unsafePerformIO newWish
{-# NOINLINE wish #-}

tixAvailable :: Bool
tixAvailable = IOExts.unsafePerformIO isTixAvailable

newWish :: IO Wish
newWish =
   do
      wishPath <- getWishPath
      childProcess <- newChildProcess wishPath [linemode True]
      let 
         writeWish = sendMsgRaw childProcess

         packStr :: (ByteArray Int -> Int -> a) -> (String -> a)
         packStr toDo str =
            let
               packed = packString str
               len = length str
            in
               toDo packed len

      -- Set up initial wish procedures.
      (packStr writeWish) (
         "proc ConvertTkValue val {" ++
            -- The "regsub" commands replace \ by \\ and newline by \\.
            "regsub -all {\\\\} $val {\\\\\\\\} res1;" ++
            "regsub -all \\n $res1 {\\\\n} res;" ++
            "return $res" ++
            "};" ++
         "proc evS x {" ++
            "set status [catch {eval $x} res];" ++
            "set val [ConvertTkValue $res];" ++ 
            "if {$status == 0} {puts \"OK $val\"} else {puts \"ER $val\"}" ++
            "};" ++
         "proc relay {evId val} {" ++ 
            "set res [ConvertTkValue $val];" ++
            "puts \"CO $evId $res\"" ++
            "};" ++
         -- The following Tcl functions adds and removes bindings for
         -- a widget.
         -- ldelete deletes an item from a list
         -- (Stolen from Tcl book page 58)
         "proc ldelete {list value} {" ++
            "set ix [lsearch -exact $list $value];" ++
            "if {$ix >=0 } {" ++
               "return [lreplace $list $ix $ix]" ++
               "} else {return $list}};" ++
         -- addtag adds a bind tag for a widget
         "proc addtag {widget tag} {" ++
            "set x [bindtags $widget];" ++
            "lappend x $tag;" ++
            "bindtags $widget $x};" ++
         -- rmtag removes a bind tag from a widget
         "proc rmtag {widget tag} {" ++
            "bindtags $widget [ldelete [bindtags $widget] $tag]}\n"  
         )
      -- get readWish reactor going.
      (readWish,destroyReadWish) <- readWishEvent childProcess
      -- set up the channels
      commands <- newChannel
      responses <- newChannel
      eventQueue <- newEqGuardedChannel
      coQueue <- newEqGuardedChannel
      bindTags <- newMVar nullBindTag
      let
         destroyWish1 =
            do
               destroyReadWish
               destroy childProcess
               -- Wish reactor will be garbage collected.
      destroyWish <- doOnce destroyWish1
      writeWishMVar <- newMVar writeWish
      let
         wish = Wish {
            commands = commands,
            responses = responses,
            eventQueue = eventQueue,
            coQueue = coQueue,
            bindTags = bindTags,
            readWish = readWish,
            writeWishMVar = writeWishMVar,
            destroyWish = destroyWish
            }
      spawnEvent eventForwarder

      return wish

-- isTixAvailable is used to determine if Tix is available . . .
isTixAvailable :: IO Bool
isTixAvailable =
   do
      OK response <- evalCmd "info commands tix"
      -- Match failure here indicates error in command "info complete tix";
      -- how can this be??
      case response of
         "tix" -> return True
         "" -> return False
         -- Match failure here indicates more than one string matching
         -- "tix", which is puzzling.

eventForwarder :: Event ()
eventForwarder = forever handleEvent
   where
      rWish = readWish wish

      -- event that passes on EV and CO events.
      handleEvent :: Event ()
      handleEvent =
            (do
               -- Pass on ev events.
               (_,evString) <- toEvent (rWish |> Eq EVType)
               noWait(send (eventQueue wish) (parseEVString evString))
            )
         +> (do
               -- Handle co events
               (_,coString) <- toEvent (rWish |> Eq COType)
               noWait(send (coQueue wish) (parseCallBack coString))
            )


readWishEvent :: ChildProcess 
   -> IO (GuardedEvent (EqMatch TclMessageType) (TclMessageType,String),
      IO())
readWishEvent childProcess =
   do
      wishInChannel <- newEqGuardedChannel
      destroy <- spawnEvent(forever(
         do
            next <- always (readMsg childProcess)
            send wishInChannel (typeWishAnswer next)
         ))
      return (listen wishInChannel,destroy)

-- typeWishAnswer parses answers from Wish.
-- The format of messages from Wish, after we've defined the first
-- few procedures, is
-- OK [escaped string]
--    for a successfully completed command with this result.
-- ER [escaped string]
--    for an unsuccessful command with this result.
-- CO [escaped string]
--    for an output of the "relay" procedure, which we use in commands
--    attached to Tcl widgets.  (EG it might be the text the user
--    has typed into a text widget.)
--    We will break this up further with splitCO, which expects to
--    find a space.
-- EV [non-escaped string]
--    for an event.  This is set up by the bind command in
--    TkCommands.hs.  (We don't need to escape this as it can't
--    contain funny characters.)
-- We parse this, unescaping the Strings where necessary.
typeWishAnswer :: String -> (TclMessageType,String)
-- typeWishAnswer returns the type, and the unescaped String.
typeWishAnswer str =
   case str of
      'O':'K':' ':rest -> (OKType,unEscape rest)
      'E':'R':' ':rest -> (ERType,unEscape rest)
      'C':'O':' ':rest -> (COType,unEscape rest)
      'E':'V':' ':rest -> (EVType,rest)
      _ -> parseError str
   where
      unEscape "" = ""
      unEscape ('\\':'n':rest) = '\n':unEscape rest
      unEscape ('\\':'\\':rest) = '\\':unEscape rest
      unEscape ('\\':_:rest) = parseError str
      unEscape (ch:rest) =ch:unEscape rest

parseError :: String -> a
parseError str = error ("Wish: couldn't parse wish response "++ (show str))


-- -----------------------------------------------------------------------
-- BindCmd and its cousins
-- -----------------------------------------------------------------------

newtype BindTag = BindTag Int deriving (Eq,Ord)

bindTagS :: BindTag -> String
bindTagS (BindTag i) = show i

bindTagR :: String -> BindTag
bindTagR str =
   case reads str of
      [(i,"")] -> BindTag i
      _ -> error ("Can't parse bind tag "++str++".")

nullBindTag :: BindTag
nullBindTag = BindTag 0

succBindTag :: BindTag -> BindTag
succBindTag (BindTag n) = BindTag (n+1)


-- -----------------------------------------------------------------------
-- Bind Event Data
-- -----------------------------------------------------------------------

-- We do not allow general Bind commands.  All bind commands simply
-- put the result to stdout in the following format:
-- "EV [bindTag]( [id][value])*"
-- where 
-- [bindTag] is our tag for the binding.
-- [id] is a single character identifying the information
-- [value] is the String value
-- In the above, (...)* means that the syntax within brackets can be
-- repeated any number n>=0 times.
-- We also assume that all the strings ([widgetTag], [String]) do not
-- contain any spaces funny escape characters.  This can be deduced from
-- page 298 and the fact that bindTags are in fact just going to
-- be lists of numbers separated by period.
mkBoundCmdArg :: BindTag -> EventInfoSet -> String
-- Make the command to be passed as ?bindstr? for "bi".
-- (See notes at the head of this section.)
mkBoundCmdArg bindTag eventInfoSet =
  let bindStr = "EV " ++ bindTagS bindTag ++
                foldr (\ par soFar -> let tag = epToChar par
                                      in ' ':tag:'%':tag:soFar)
                      "" (listEventInfoSet eventInfoSet)
  in "{puts " ++ (delimitString bindStr) ++ "}"

parseEVString :: String -> (BindTag,EventInfo)
-- parseEVString parses the resulting String, EXCEPT for the
-- initial "EV ", which are stripped off before they get this far.
parseEVString str =
   let
      bindTagStr:settings = words str
      eventInfo = mkEventInfo
         (map (\ (tag:rest) -> (epFromChar tag,rest)) settings)
   in
     (bindTagR bindTagStr,eventInfo)

-- -----------------------------------------------------------------------
-- Wish events
-- These also have to implement Eq and Ord for the benefit of
-- the Widget dispatcher, which needs to handle finite maps on them.
-- NB - we won't encourage people to get hold of Wish's current
-- name for an event, since this is hard to reconcile to our
-- encapsulation (EG it probably involves detailed knowledge of
-- local keysyms to do it properly). 
-- -----------------------------------------------------------------------

data WishEvent = WishEvent [WishEventModifier] WishEventType 
   deriving (Ord,Eq)

instance Show WishEvent where
   -- We specify that the resulting String is already escaped
   -- as necessary.
   showsPrec _ (WishEvent modifiers wishEventType) acc =
    '<' : 
       (foldr 
          (\ modifier soFar -> showP modifier ('-':soFar))
          (typeToStringP wishEventType ('>':acc))
          modifiers
          )

-- page 290 except that we merge keysyms (page 291) into the KeyPress and 
-- KeyRelease type.
data WishEventType =
   Activate | 
   ButtonPress (Maybe BNo) | ButtonRelease (Maybe BNo) | 
   Circulate |
   Colormap | Configure | Deactivate | Destroy | Enter | Expose |
   FocusIn | FocusOut | Gravity | 
   KeyPress (Maybe KeySym) | KeyRelease (Maybe KeySym)|
   Motion | Leave | Map | Property | Reparent | Unmap | 
   Visibility deriving (Show,Eq,Ord) 
   -- the Show instance won't work for KeySyms, so we fix up later

newtype KeySym = KeySym String deriving (Show,Ord,Eq)
-- A KeySym can be a single character representing a key.  However others
-- are defined, and depend on the window implementation.  For example,
-- on this machine the Return key is called "Return", and the 
-- Enter key "KP_Enter".  Page291 has a wish binding for determining
-- the keysym for a key.
-- The KeySym is escaped as necessary before being fed to Wish;
-- for example KeySym ['\n'] works.

ksToStringP :: Maybe KeySym -> String -> String
ksToStringP Nothing acc = acc
ksToStringP (Just (KeySym keySym)) acc =
   '-':((escapeString keySym)++acc)

newtype BNo = BNo Int deriving (Eq,Ord,Show) -- used for buttons

bNoToStringP :: Maybe BNo -> String -> String
bNoToStringP Nothing acc = acc
bNoToStringP (Just (BNo bNo)) acc = '-':(showP bNo acc)

typeToStringP :: WishEventType -> String -> String
typeToStringP (ButtonPress bNo) acc = 
   "ButtonPress" ++ (bNoToStringP bNo acc)
typeToStringP (ButtonRelease bNo) acc = 
   "ButtonRelease" ++ (bNoToStringP bNo acc)
typeToStringP (KeyPress ks) acc = "KeyPress" ++ (ksToStringP ks acc)
typeToStringP (KeyRelease ks) acc = "KeyRelease" ++ (ksToStringP ks acc)
typeToStringP other acc = showP other acc

-- page 294
-- We rename "Command" "CommandKey" to avoid conflicts with the
-- Command attribute.
data WishEventModifier =
   Control | Shift | Lock | CommandKey | Meta | M | Alt | Mod1 |
   Mod2 | Mod3 | Mod4 | Mod5 |
   Button1 | Button2 | Button3 | Button4 | Button5 | 
   Double | Triple deriving (Show,Ord,Eq)


-- -----------------------------------------------------------------------
-- CallBackId's identify callbacks.
-- -----------------------------------------------------------------------

newtype CallBackId = CallBackId ObjectID deriving (Eq,Ord,Show)

parseCallBack :: String -> (CallBackId, ())
parseCallBack str =
  case reads str of
    [(i, _)] -> (CallBackId i, ())
    _ -> error ("Couldn't parse Wish callback "++str)

showCallBackId :: CallBackId -> String
showCallBackId (CallBackId nm) = show nm


-- -----------------------------------------------------------------------
-- General abbreviations
-- -----------------------------------------------------------------------

-- Convenient abbreviation
showP :: Show a => a -> String -> String
showP val acc = showsPrec 0 val acc