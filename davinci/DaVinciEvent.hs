{- #########################################################################

MODULE        : DaVinciEvent
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : DaVinci Events.

FileMenuInvocation's look bugged.  Change the read action.

   ######################################################################### -}


module DaVinciEvent (
        DaVinciAnswer(..),
        decodeDaVinciAnswer, -- :: String -> IO DaVinciAnswer
        DaVinciEvent(..),
        DaVinciFileEvent(..),
        DaVinciEventInfo(..)
        ) where

-- import Char(isSpace)
import Exception

import Dynamics
import ExtendedPrelude
import Debug(debug)

import SIM

import DaVinciGraphTerm(NodeId(..),EdgeId(..),MenuItemId(..))


-- ---------------------------------------------------------------------------
-- DaVinci Events
-- ---------------------------------------------------------------------------

data DaVinciEvent =
          Ok
        | ComError
        | InternalError
        | ContextChanged
        | NodeSelectionLabels
        | NodeDoubleClick
        | EdgeSelectionLabel
        | EdgeDoubleClick
        | MenuSelection
        | FileMenuSelection
        | PopupSelectionNode
        | PopupSelectionEdge
        | IconSelection
        | CreateNode
        | CreateNodeAndEdge
        | CreateEdge
        | DisConnect
        | Close
        | LastGraphClosed
        deriving (Eq, Ord, Read, Show, Enum)


-- ---------------------------------------------------------------------------
-- DaVinci Event Info
-- ---------------------------------------------------------------------------

data DaVinciEventInfo =
          NoDaVinciEventInfo
        | NodeSelection [NodeId]
        | EdgeSelection EdgeId
        | MenuInvocation MenuItemId
        | PopupSelectionNodeInf NodeId MenuItemId
        | PopupSelectionEdgeInf EdgeId MenuItemId
        | FileMenuInvocation DaVinciFileEvent
        | Context Int
        | Reply String

daVinciEventInfoT :: TyCon 
daVinciEventInfoT = mkTyCon "DaVinciEvent" "DaVinciEventInfo"

instance Typeable DaVinciEventInfo where
   typeOf _ = mkTypeTag daVinciEventInfoT []
        


-- ---------------------------------------------------------------------------
-- DaVinci Answers
-- ---------------------------------------------------------------------------

data DaVinciAnswer = DaVinciAnswer DaVinciEvent DaVinciEventInfo

decodeDaVinciAnswer :: String -> IO DaVinciAnswer
decodeDaVinciAnswer daVinciAnswerStr =
   do 
      exceptionOrResult <- tryAll doParse
      case exceptionOrResult of 
         Left exception ->
            return (DaVinciAnswer InternalError (Reply
               (daVinciAnswerStr ++ "\n" ++ (show exception))
               ))
         Right result -> return result
   where
      menuItem :: String -> MenuItemId
      menuItem str = MenuItemId (read str) 
      menuInvocation :: String -> DaVinciEventInfo
      menuInvocation str = MenuInvocation(menuItem str)
      
      doParse = case daVinciParse daVinciAnswerStr of
         ("ok",[]) -> DaVinciAnswer Ok (Reply "")
         ("quit",[]) -> DaVinciAnswer DisConnect NoDaVinciEventInfo
         ("disconnect",[]) -> 
            DaVinciAnswer DisConnect NoDaVinciEventInfo
         ("close",[]) -> DaVinciAnswer Close NoDaVinciEventInfo
         ("context",[cNumber]) ->
            DaVinciAnswer ContextChanged (Context (read cNumber))
         ("node_selections_labels",nodeLabelStrings) ->
            DaVinciAnswer NodeSelectionLabels 
               (NodeSelection (map NodeId nodeLabelStrings))
         ("edge_selection_label",[edgeLabelString]) ->
            DaVinciAnswer EdgeSelectionLabel 
               (EdgeSelection (EdgeId edgeLabelString))
         ("node_double_click",[]) ->
            DaVinciAnswer NodeDoubleClick NoDaVinciEventInfo
         ("edge_double_click",[]) ->
            DaVinciAnswer EdgeDoubleClick NoDaVinciEventInfo
         ("icon_selection",[iconString]) ->
            DaVinciAnswer IconSelection (menuInvocation iconString)
         ("popup_selection_node",[nodeLabelString,menuItemString]) ->
            DaVinciAnswer PopupSelectionNode 
               (PopupSelectionNodeInf 
                  (NodeId nodeLabelString) (menuItem menuItemString))
         ("popup_selection_edge",[edgeLabelString,menuItemString]) ->
            DaVinciAnswer PopupSelectionEdge
               (PopupSelectionEdgeInf 
                  (EdgeId edgeLabelString) (menuItem menuItemString))
         ("menu_selection",[menuItemString]) ->
               case menuItemString of
                  '#':fileEventStr ->
                     DaVinciAnswer FileMenuSelection
                        (FileMenuInvocation (read fileEventStr))
                  _ -> DaVinciAnswer MenuSelection 
                     (menuInvocation menuItemString)
         ("create_node_and_edge",[nodeIdString]) ->
             DaVinciAnswer CreateNodeAndEdge 
                (NodeSelection [NodeId nodeIdString])
         ("create_node",[]) -> 
            DaVinciAnswer CreateNode NoDaVinciEventInfo
         ("create_edge",nodeIdStrings) ->
            DaVinciAnswer CreateEdge 
               (NodeSelection (map NodeId nodeIdStrings))
         ("communication_error",[comError]) ->
               DaVinciAnswer ComError (Reply comError)
         _ -> error "can't parse daVinci answer (2)"


daVinciParse :: String -> (String,[String])
-- All DaVinci Strings have one of the following forms:
-- (a) a string containing no left parentheses, in which case daVinciParse
--     the string with an empty list
-- (b) a string with no left parentheses, followed by (, a sequence of
--     one or more C strings separated by commas, followed by ),
--     in which case we return the first string, paired with the paired
--     C strings
-- (c) a string with no left parentheses, followed by ([, a sequence
--     of one or more C strings separated by commans, followed by )].
--     In which case we return as in (b).  (This case only occurs for
--     node_selections_labels.)
-- In effect we merge (b) and (c) together by ignoring a single [ or ]
-- at the start and end of the arguments, respectively.
-- When we have errors we raise "error".

daVinciParse "" = ("",[])
daVinciParse ('(':rest) =
   let
      args =
         case rest of
           '[':argsString -> parseArgs argsString
           argsString -> parseArgs argsString
   in
      ("",args)
   where
      parseArgs :: String -> [String]
      parseArgs argsString =
         case reads argsString of
            [(str,")")] -> [str]
            [(str,")]")] -> [str]
            [(str,',':rest)] ->
               let
                  restArgs = parseArgs rest
               in
                  (str:restArgs)
            _ -> error "can't parse daVinci answer (1)"
daVinciParse (ch:rest) =
   let
      (fname,args) = daVinciParse rest
   in
      (ch:fname,args)



-- ---------------------------------------------------------------------------
-- File Menu Events
-- ---------------------------------------------------------------------------

data DaVinciFileEvent =
          FileNew
        | FileOpen
        | FileSave
        | FileSaveAs
        | FilePrint
        | FileClose
        | FileExit
        deriving (Eq, Ord)
-- Example answer for this:
-- menu_selection("#%new")

instance Read DaVinciFileEvent where
   readsPrec p b =
     case b of
        '%':'n':'e':'w':xs -> [(FileNew,xs)]
        '%':'o':'p':'e':'n':xs -> [(FileOpen,xs)]
        '%':'s':'a':'v':'e':'a':'s':xs -> [(FileSave,xs)]
        '%':'s':'a':'v':'e':xs -> [(FileSave,xs)]
        '%':'p':'r':'i':'n':'t':xs -> [(FilePrint,xs)]
        '%':'c':'l':'o':'s':'e':xs -> [(FileClose,xs)]
        '%':'e':'x':'i':'t':xs -> [(FileExit,xs)]
        _ -> []

instance Show DaVinciFileEvent where
   showsPrec d p r = 
      (case p of 
        FileNew -> "%new" 
        FileOpen -> "%open"
        FileSave -> "%save"
        FileSaveAs -> "%saveas"
        FilePrint -> "%print"
        FileClose -> "%close"
        FileExit -> "%exit"
        ) ++ r




