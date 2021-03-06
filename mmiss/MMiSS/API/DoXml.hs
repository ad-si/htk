-- | This module contains the code which actually processes and
-- distributes XML requests.
module MMiSS.API.DoXml(
   doXml, -- :: Handle -> IO ()
   ) where

import IO
import Data.List

import System.IO.Unsafe

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types


import Util.Computation
import Util.Messages
import Util.WBFiles
import Util.AtomString

import Events.Synchronized

import Reactor.BSem
import Reactor.Lock

import Server.PasswordFile

import MMiSS.DTD hiding (validateElement)


import MMiSS.API.SecurityOps
import MMiSS.API.CallServer
import MMiSS.API.CheckOutCommit
import MMiSS.API.GetPut
import MMiSS.ImportExportErrors

import MMiSS.API.Request
import MMiSS.API.SessionState
import MMiSS.API.Messages
import MMiSS.API.Block

doXml :: Handle -> User -> IO ()
doXml handle user =
   do
      state <- newSessionState

      (result :: Either String ())
         <- catchErrors (doRequests state handle user)

      case result of
         Right () -> done
         Left mess -> signalError handle Messages_status_panic mess

      hClose handle

-- --------------------------------------------------------------------------
-- Handle a single request.
-- --------------------------------------------------------------------------

-- doRequests does the MMiSS requests until EOF.
doRequests :: MMiSSSessionState -> Handle -> User -> IO ()
doRequests state handle user =
   do
      (result :: Either String ()) <- catchErrors (
         do
            (request,block) <- readRequest handle

            block <- doRequests2 state handle user block request
            writeResponse handle block
         )

      case result of
         Right () -> done
         Left mess ->
            signalError handle Messages_status_fail mess

      doRequests state handle user


doRequests2 :: MMiSSSessionState -> Handle -> User -> Block -> Request
   -> IO Block
doRequests2 state handle user blockIn request =
   -- Get hold of the lock, which will protect some other client from
   -- simultaneously trampling on the messFns.
   synchronize requestLock (
      do
         (Messages _ mess0) <- getMessages state
         if null mess0
            then
               done
            else
               putStrLn ("Mysterious extra messages " ++ show mess0)
         setMessFns (apiMessFns state)

         let
            pn x = return (x,Nothing)

         (oneOf,blockOutOpt) <- case request of
            RequestConnect command ->
               do
                  response <- connect state command user
                  pn (OneOf12 response)
            RequestCloseServer command ->
                do
                  response <- closeServer state command
                  pn (TwoOf12 response)
            RequestListVersions command ->
                do
                  response <- listVersions state command
                  pn (ThreeOf12 response)
            RequestCheckOut command ->
                do
                  response <- checkOut state command
                  pn (FourOf12 response)
            RequestChangeUserInfo command ->
                do
                  response <- changeUserInfo state command
                  pn (FiveOf12 response)
            RequestCommitVersion command ->
                do
                  response <- commitVersion state command
                  pn (SixOf12 response)
            RequestCloseVersion command ->
                do
                  response <- closeVersion state command
                  pn (SevenOf12 response)
            RequestGetObject command ->
                do

                  (response,blockOut) <- getObject state command dummyBlock
                  return (EightOf12 response,Just blockOut)
            RequestPutObject command ->
                do
                  response <- putObject state command blockIn
                  pn (NineOf12 response)
            RequestGetPermissions command ->
                do
                   response <- getPermissions state command
                   pn (TenOf12 response)
            RequestSetPermissions command ->
                do
                   response <- setPermissions state command
                   pn (ElevenOf12 response)
            RequestSetAdminStatus command ->
                do
                   response <- setAdminStatus state command
                   pn (TwelveOf12 response)



         messages <- getMessages state
         let
            response = Response messages (Just oneOf)

            block = case blockOutOpt of
               Nothing -> toSimpleBlock response
               Just blockOut0 -> setResponse blockOut0 response

         return block
      )

-- We lock our access to the MessFns.
requestLock :: BSem
requestLock = unsafePerformIO newBSem
{-# NOINLINE requestLock #-}


-- --------------------------------------------------------------------------
-- Handling parsing errors and so on.
-- --------------------------------------------------------------------------

-- catchErrors also catches other Haskell exceptions.
catchErrors :: IO a -> IO (Either String a)
catchErrors act = catchImportExportErrors (makeOtherExcepsToOurs act)

-- --------------------------------------------------------------------------
-- Displaying errors
-- --------------------------------------------------------------------------


-- Fall-out when everything goes wrong.
signalError :: Handle -> Messages_status -> String -> IO ()
signalError handle status string = signalErrors handle status [string]

signalErrors :: Handle -> Messages_status -> [String] -> IO ()
signalErrors handle status messes =
   do
      let
         messages = Messages
            (Messages_Attrs {
               messagesStatus = NonDefault status
               })
            (map (\ mess -> (Messages_Error (Error mess))) messes)

         response = Response messages Nothing

      writeResponse handle (toSimpleBlock response)

-- --------------------------------------------------------------------------
-- Reading and Writing XML.  These functions also handle escaping
-- / unescaping.
-- --------------------------------------------------------------------------

readRequest :: Handle -> IO (Request,Block)
readRequest handle =
   do
      block <- readBlock handle
      requestStr <- case lookupBlockData block 0 of
         Just (BlockData {blockType = 0,blockText = icsl}) ->
            return (toString icsl)
         _ -> importExportError
            "Request block does not begin with an XML element"
      let
         Document _ _ elementE _ = xmlParse "MMiSS API input" requestStr

         element = xmlUnEscape stdXmlEscaper elementE

      case validateElement "request" element of
         [] -> done
         errors -> importExportError (unlines errors)

      case fromElem [CElem element] of
         (Just request,[]) -> return (request,block)
         (Nothing,_) -> importExportError ("No request found in "
            ++ requestStr)
         (_,_) -> importExportError ("Unwanted extra input found in "
            ++ requestStr)


writeResponse :: Handle -> Block -> IO ()
writeResponse handle block =
   do
      writeBlock handle block
      hFlush handle

toSimpleBlock :: Response -> Block
toSimpleBlock response =
   fst (addBlockData emptyBlock (toResponseBlockData response))

dummyBlock :: Block
dummyBlock = fst (addBlockData emptyBlock dummyBlockData)

setResponse :: Block -> Response -> Block
setResponse block response =
   setBlockData block 0 (toResponseBlockData response)

toResponseBlockData :: Response -> BlockData
toResponseBlockData response =
   let
      [CElem element] = toElem response

      icsl = fromString (toUglyExportableXml element)
   in
      BlockData {blockType = 0,blockText = icsl}




-- --------------------------------------------------------------------------
-- Validation
-- --------------------------------------------------------------------------

-- stolen from MMiSSDTD.hs
validateElement :: String -> Element -> [String]
validateElement elementName (element @ (Elem name _ _)) =
   if name /= elementName
      then
         ["Expected a "++elementName++" but found a "++name]
      else
         (simpleDTD theAPIDTD) element


theAPIDTD :: MMiSSDTD
theAPIDTD = unsafePerformIO getTheAPIDTD
{-# NOINLINE theAPIDTD #-}

getTheAPIDTD :: IO MMiSSDTD
getTheAPIDTD =
   do
      (Just filePath) <- getMMiSSAPIDTD
      readDTD filePath
