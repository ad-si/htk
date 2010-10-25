{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
-- -----------------------------------------------------------------------
module HTk.Kernel.Wish (

  wish,

  evalTclScript,
  execTclScript,
  execCmd,
  evalCmd,
  -- escape,
  -- delimitString,
  Wish(..),
  TclCmd,
  TclScript,
  TclMessageType(..),
  BindTag,
  bindTagS,
  succBindTag,
  WishEvent(..),
  WishEventModifier(..),
  WishEventType(..),
  mkBoundCmdArg,
  KeySym(..),
  CallBackId(..),
  showP,

  requirePackage,       -- :: String -> IO(Bool).  Try to load a package.
  forgetPackage,        -- :: String -> IO().      Forget a package.
  isPackageAvailable,   -- :: String -> IO(Bool).  True if package loaded.
  isTixAvailable,       -- :: IO Bool.  True if we are using tixwish, which
                        -- means it was successfully loaded with requirePackage
  cleanupWish,

  delayWish, -- :: IO a -> IO a
     -- delayWish does an action, with the proviso that wish commands
     -- executed within the action by this or any other thread
     -- may be delayed.  This can (allegedly) be faster.

) where

-- The preprocessor symbol ASYNC_WISH_ERRORS, if non-zero, causes wish
-- errors to be handled asynchronously.  This is done by default unless DEBUG
-- is set.
-- This is an optimisation.  The possible bad consequences are that should
-- wish itself produce an error,
-- (1) we may execute some additional wish commands before detecting it.
-- (2) it is hard to associate the error with the command which provoked it.
-- On the other hand, it saves us having to wait for acknowledgment of
-- commands, which particularly on Windows (where we have to access wish output
-- by polling with the current version of ghc, 5.02.2) should save a lot of
-- time.
#ifndef ASYNC_WISH_ERRORS
#ifdef DEBUG
#define ASYNC_WISH_ERRORS 0
#else
#define ASYNC_WISH_ERRORS 1
#endif
#endif


import Data.List(union,delete)

import Control.Concurrent
import Foreign.C.String
import System.IO.Unsafe
import Control.Exception

import Util.Object
import Util.Computation

import Events.Events
import Events.GuardedEvents
import Events.EqGuard
import Events.Destructible
import Events.Synchronized

import Reactor.BSem
import Reactor.InfoBus

import Reactor.ReferenceVariables
import HTk.Kernel.EventInfo

import HTk.Kernel.GUIValue
import HTk.Kernel.CallWish

-- -----------------------------------------------------------------------
-- basic execution of Tcl commands
-- -----------------------------------------------------------------------

---
-- evalCmd is used for commands which expect an answer,
-- and calls evalTclScript.
evalCmd :: TclCmd -> IO String
evalCmd cmd = evalTclScript [cmd]

---
-- Used for commands which expect an answer.
evalTclScript :: TclScript -> IO String
evalTclScript script =
   do
      let buffer = bufferedCommands wish
      -- (1) look at the buffer, execute the contents, and empty it.
      bufferContents <- takeMVar buffer
      case bufferContents of
         (0,_) -> putMVar buffer bufferContents
         (n,[]) -> putMVar buffer bufferContents
         (n,script) ->
            do
               putMVar buffer (n,[])
               execCmdInner (reverse script)
      -- (2) execute the command
      response <- evalCmdInner script
      doResponse response


---
-- execCmd is used for commands which don't expect an answer
-- and calls execTclScript
execCmd :: TclCmd -> IO ()
execCmd cmd = execTclScript [cmd]

---
-- Used for commands which do not expect an answer
execTclScript :: TclScript -> IO ()
execTclScript script =
   do
      let buffer = bufferedCommands wish
      bufferContents <- takeMVar buffer
      case bufferContents of
         (0,_) -> -- just do it
            do
               putMVar buffer (0,[])
               execCmdInner script
               done
         (n,buffered) -> -- don't do it
            do
               let
                  revAppend [] ys = ys
                  revAppend (x:xs) ys = revAppend xs (x:ys)
               putMVar buffer (n,revAppend script buffered)

---
-- delayWish does an action, with the proviso that wish commands
-- executed within the action by this or any other thread
-- may be delayed.  This can (allegedly) be faster.
delayWish :: IO a -> IO a
delayWish action =
   do
      beginBuffering
      tried <- Control.Exception.try action
      endBuffering
      propagate tried

---
-- beginBuffering begins buffering commands (if we aren't already).
beginBuffering :: IO ()
beginBuffering =
   do
      let buffer = bufferedCommands wish
      bufferContents <- takeMVar buffer
      case bufferContents of
         (n,script) -> putMVar buffer (n+1,script)

---
-- unbuffercommands undoes a beginBuffering, and flushes the current buffer.
endBuffering :: IO ()
endBuffering =
   do
      let buffer = bufferedCommands wish
      bufferContents <- takeMVar buffer
      case bufferContents of
         (n,script) ->
            do
               execCmdInner (reverse script)
               putMVar buffer (n-1,[])

---
-- evalCmdInner takes a (possibly empty) TclScript and executes it,
-- returning a response.  It does not look at the buffer, so should
-- not be called from outside.
evalCmdInner :: TclScript -> IO TclResponse
evalCmdInner [] = return (OK "")
evalCmdInner tclScript =
   do
      let
         scriptString = foldr1 (\ cmd s -> cmd ++ (';':s)) tclScript
         cmdString = "evS " ++ escape scriptString ++"\n"

      withCStringLen cmdString evalCmdPrim

---
-- This is the most primitive command evaluator and does not
-- look at the buffer.  So it shouldn't be called from outside.
evalCmdPrim :: CStringLen -> IO TclResponse
evalCmdPrim cStringLen =
   do
      let
         rWish = readWish wish
         wWish = writeWish wish
      synchronize (wishLock wish) (
         do
            wWish cStringLen
            sync(
                  toEvent (rWish |> Eq OKType) >>>=
                     (\ (_,okString) -> return (OK okString))
#if ! ASYNC_WISH_ERRORS
               +> toEvent (rWish |> Eq ERType) >>>=
                     (\ (_,erString) -> return (ER erString))
#endif
               )
         )

#if ASYNC_WISH_ERRORS
---
-- execCmdInner corresponds to evalCmdInner, but does not return a response.
execCmdInner :: TclScript -> IO ()
execCmdInner [] = done
execCmdInner tclScript =
   do
      let
         scriptString = foldr1 (\ cmd s -> cmd ++ (';':s)) tclScript
         cmdString = "exS " ++ escape scriptString ++"\n"
         -- The difference is we call "exS" and not "evS".

      withCStringLen cmdString execCmdPrim

---
-- execCmdPrim corresponds to evalCmdPrim, but does not wait for a response.
execCmdPrim :: CStringLen -> IO ()
execCmdPrim cStringLen = writeWish wish cStringLen

#else

execCmdInner :: TclScript -> IO ()
execCmdInner script =
   do
      response <- evalCmdInner script
      doResponse1 response
      done

#endif


doResponse :: TclResponse -> IO String
doResponse (OK res) = return res
#if ! ASYNC_WISH_ERRORS
doResponse (ER err) = error err
#endif

doResponse1 :: TclResponse -> IO ()
doResponse1 (OK res) = return ()
#if ! ASYNC_WISH_ERRORS
doResponse1 (ER err) =
   do
      fingersCrossed err
      done
#endif

fingersCrossed :: String -> IO ()
fingersCrossed err =
   putStrLn ("Unexpected error " ++ err ++ " returned from wish\n"
      ++ "Continuing, with fingers crossed")

-- -----------------------------------------------------------------------
-- wish datatypes
-- -----------------------------------------------------------------------

data Wish = Wish {
   wishLock :: BSem,
      -- this locks wish when a command has been sent but not answer
      -- received, as yet.

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

   writeWish :: CStringLen -> IO (),
      -- Command to execute a Wish command.

   destroyWish :: IO (), -- Command to destroy this Wish instance.


   bufferedCommands :: MVar (Int,TclScript),
      -- The integer indicates if buffering is going on.
      --    If non-zero it is.  When we start new buffering, we
      --    increment the integer.
      -- The TclScript contains the current contents of the buffer
      --    IN REVERSE ORDER.
      -- If the integer is 0, the TclScript is [].
   oID :: ObjectID
   }

type TclCmd = String
type TclScript = [TclCmd]

#if ASYNC_WISH_ERRORS
newtype TclResponse = OK String
#else
data TclResponse = OK String | ER String
#endif

data TclMessageType = OKType | ERType | COType | EVType deriving (Eq,Ord,Show)


-- ----------------------------------------------------------------
-- wish instances
-- ----------------------------------------------------------------

instance Object Wish where
   objectID wish = oID wish

instance Destroyable Wish where
   destroy wish = destroyWish wish


-- ----------------------------------------------------------------
-- running the wish
-- ----------------------------------------------------------------

wish :: Wish
wish = unsafePerformIO newWish
{-# NOINLINE wish #-}

cleanupWish :: IO ()
cleanupWish = destroy wish

newWish :: IO Wish
newWish =
   do
      calledWish <- callWish
      let
         writeWish = sendCalledWish calledWish

      -- Set up initial wish procedures.
         wishHeader =
            "proc ConvertTkValue val {" ++
               -- The "regsub" commands replace \ by \\ and newline by \\.
               "regsub -all {\\\\} $val {\\\\\\\\} res1;" ++
               "regsub -all \\n $res1 {\\\\n} res;" ++
               "return $res" ++
               "};" ++
-- Execute the command, returning the result.
            "proc evS x {" ++
               "set status [catch {eval $x} res];" ++
               "set val [ConvertTkValue $res];" ++
               "if {$status == 0} {puts \"OK $val\"} else {puts \"ER $val\"}" ++
               "};" ++
#if ASYNC_WISH_ERRORS
-- Execute the command, not returning the result.
            "proc exS x {" ++
               "set status [catch {eval $x} res];" ++
               "if {$status} {puts [concat \"EX \" [ConvertTkValue $res]]}" ++
               "};" ++
#endif
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

      withCStringLen wishHeader writeWish

      -- get readWish reactor going.
      (readWish,destroyReadWish) <- readWishEvent calledWish
      -- set up the channels
      wishLock <- newBSem
      eventQueue <- newEqGuardedChannel
      coQueue <- newEqGuardedChannel
      bindTags <- newMVar nullBindTag
      let
         destroyWish1 =
            do
               destroyReadWish
               destroyCalledWish calledWish
               -- Wish reactor will be garbage collected.
      destroyWish <- doOnce destroyWish1
      bufferedCommands <- newMVar (0,[])
      oID <- newObject
      let
         wish = Wish {
            wishLock = wishLock,
            eventQueue = eventQueue,
            coQueue = coQueue,
            bindTags = bindTags,
            readWish = readWish,
            writeWish = writeWish,
            destroyWish = destroyWish,
            bufferedCommands = bufferedCommands,
            oID = oID
            }
      _ <- spawnEvent eventForwarder

      registerToolDebug "Wish" wish -- so that shutdown works.

      return wish

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
#if ASYNC_WISH_ERRORS
         +> (do
               -- Handle wish errors
               (_,erString) <- toEvent (rWish |> Eq ERType)
               always (fingersCrossed erString)
            )
#endif


readWishEvent :: CalledWish
   -> IO (GuardedEvent (EqMatch TclMessageType) (TclMessageType,String),
      IO())
readWishEvent calledWish =
   do
      wishInChannel <- newEqGuardedChannel
      destroy <- spawnEvent(forever(
         do
            next <-
               always (Control.Exception.catch (readCalledWish calledWish)                                    (\ (_ :: SomeException) -> return "OK Terminated"))
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
-- Interface to wish packages
-- -----------------------------------------------------------------------

loadedPackages :: Ref [String]
loadedPackages = unsafePerformIO (newRef [])
{-# NOINLINE loadedPackages #-}

-- Require a package, returning flag for success
requirePackage :: String -> IO (Bool)
requirePackage package =
       do response <- evalCmd ("package require " ++ package)
          if response == ("can't find package " ++ package)
             then return False
             else do loaded <- getRef loadedPackages
                     setRef loadedPackages ([package] `union` loaded)
                     return True

forgetPackage :: String -> IO ()
forgetPackage package =
       do evalCmd ("package forget " ++ package)
          loaded <- getRef loadedPackages
          setRef loadedPackages (delete package loaded)
          return ()

-- isPackageAvailable is used to determine if a package is loaded
-- (must use requirePackage to load it first, if desired)
isPackageAvailable :: String -> IO Bool
isPackageAvailable package =
   do loaded <- getRef loadedPackages
      return (package `elem` loaded)

-- isTixAvailable is used to determine if Tix is available . . .
isTixAvailable :: IO Bool
isTixAvailable = isPackageAvailable "Tix"


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
mkBoundCmdArg :: BindTag -> EventInfoSet -> Bool-> String
-- Make the command to be passed as ?bindstr? for "bi".
-- (See notes at the head of this section.)
-- Adding a "break" statement behind the puts prevents the binding from being
-- processed further. That means we override earlier/default bindings.
mkBoundCmdArg bindTag eventInfoSet break =
  let bindStr = "EV " ++ bindTagS bindTag ++
                foldr (\ par soFar -> let tag = epToChar par
                                      in ' ':tag:'%':tag:soFar)
                      "" (listEventInfoSet eventInfoSet)
  in "{puts " ++ (delimitString bindStr) ++
       if break then "; break}" else "}"

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

type BNo = Int -- used for buttons

bNoToStringP :: Maybe BNo -> String -> String
bNoToStringP Nothing acc = acc
bNoToStringP (Just bNo) acc = '-':(showP bNo acc)

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

-- Like toTkString, except it only places quotes if necessary
escape :: String-> String
escape = delimitString . escapeString

-- Convenient abbreviation
showP :: Show a => a -> String -> String
showP val acc = showsPrec 0 val acc
