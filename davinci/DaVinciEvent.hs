{- #########################################################################

MODULE        : DaVinciEvent
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : DaVinci Events.


   ######################################################################### -}


module DaVinciEvent (
        DaVinciAnswer(..),
        DaVinciEvent(..),
        DaVinciFileEvent(..),

        NodeId,
        EdgeId,
        DaVinciEventInfo(..)
        ) where

import SIM
import Dynamics
import DaVinciGraphTerm(NodeId(..), EdgeId(..))
import Char(isSpace)
import ExtendedPrelude

import Debug(debug)


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
        | MenuInvocation Int
        | PopupSelectionNodeInf NodeId Int
        | PopupSelectionEdgeInf EdgeId Int
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

instance Read DaVinciAnswer where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'o':'k':xs -> 
                [(DaVinciAnswer Ok (Reply xs),"")]

        'q':'u':'i':'t':xs -> 
                [(DaVinciAnswer DisConnect NoDaVinciEventInfo,"")]

        'd':'i':'s':'c':'o':'n':'n':'e':'c':'t': xs -> 
                [(DaVinciAnswer DisConnect NoDaVinciEventInfo,"")]
        
        'c':'l': 'o':'s':'e':xs -> 
                [(DaVinciAnswer Close NoDaVinciEventInfo,"")]

        'c':'o':'n':'t':'e':'x':'t':'(':xs -> 
                [(DaVinciAnswer ContextChanged (Context cid),"")]
                where cid = (read . read) (takeWhile (\ch -> ch /= ')') xs)

        'n':'o':'d':'e':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'s':
           '_':'l':'a':'b':'e':'l':'s':'(':'[':xs  -> 
                [(DaVinciAnswer NodeSelectionLabels (NodeSelection nids),"")]
                where   labels = split (== ',') (takeWhile (\ch -> ch /= ']') xs)
                        nids = map (NodeId . read) labels

        'e':'d':'g':'e':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'_':
           'l':'a':'b':'e':'l':'(': xs  -> 
                [(DaVinciAnswer EdgeSelectionLabel (EdgeSelection eid),"")]
                where eid = EdgeId (read (takeWhile (/= ')') xs))

        'n':'o':'d':'e':'_':'d':'o':'u':'b':'l':'e':'_':
           'c':'l':'i':'c':'k':xs  -> 
                [(DaVinciAnswer NodeDoubleClick NoDaVinciEventInfo,"")]

        'e':'d':'g':'e':'_':'d':'o':'u':'b':'l':'e':'_':
           'c':'l':'i':'c':'k':xs  -> 
                [(DaVinciAnswer EdgeDoubleClick NoDaVinciEventInfo,"")]

        'i':'c':'o':'n':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'(': xs ->
                [(DaVinciAnswer IconSelection (MenuInvocation iid),"")]
                where iid = (read . read) (takeWhile (\ch -> ch /= ')') xs)

        'p':'o':'p':'u':'p':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'_':
           'n':'o':'d':'e':'(':xs -> 
                [(DaVinciAnswer PopupSelectionNode (PopupSelectionNodeInf nid mid),"")]
                where inf = takeWhile (\ch -> ch /= ')') xs
                      [snid,smid] = split (== ',') inf
                      nid = NodeId (read snid)
                      mid = (read . read) smid

        'p':'o':'p':'u':'p':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'_':
           'e':'d':'g':'e':'(':xs -> 
                [(DaVinciAnswer PopupSelectionEdge (PopupSelectionEdgeInf nid mid),"")]
                where inf = takeWhile (\ch -> ch /= ')') xs
                      [snid,smid] = split (== ',') inf
                      nid = EdgeId (read snid)
                      mid = (read . read) smid

        'm':'e':'n':'u':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'(': 
           '#':xs -> 
                [(DaVinciAnswer FileMenuSelection (MenuInvocation fid),"")]
                where fid = read( (takeWhile (\ch -> ch /= ')') xs))
                        
                        
        'm':'e':'n':'u':'_':'s':'e':'l':'e':'c':'t':'i':'o':'n':'(':xs ->
                [(DaVinciAnswer MenuSelection (MenuInvocation iid),"")]
                where iid = (read . read) (takeWhile (\ch -> ch /= ')') xs)


        'c':'r':'e':'a':'t':'e':'_':'n':'o':'d':'e':'_':'a':'n':'d':'_':
            'e':'d':'g':'e':'(':xs  -> 
                [(DaVinciAnswer CreateNodeAndEdge (NodeSelection [nid]),"")]
                where   label = takeWhile (\ch -> ch /= ')') xs
                        nid = (NodeId . read) label


        'c':'r':'e':'a':'t':'e':'_':'n':'o':'d':'e':xs  -> 
                [(DaVinciAnswer CreateNode NoDaVinciEventInfo,"")]


        'c':'r':'e':'a':'t':'e':'_':'e':'d':'g':'e':'(':xs  -> 
                [(DaVinciAnswer CreateEdge (NodeSelection nids),"")]
                where   labels = split (== ',') (takeWhile (\ch -> ch /= ')') xs)
                        nids = map (NodeId . read) labels

        'c':'o':'m':'m':'u':'n':'i':'c':'a':'t':'i':'o':'n':'_':
           'e':'r':'r':'o':'r':'(':xs -> 
                [(DaVinciAnswer ComError (Reply msg),"")]
                where msg = takeWhile (\ch -> ch /= ')') xs
        xs ->
                
                [(DaVinciAnswer InternalError (Reply xs),"")]


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

instance Read DaVinciFileEvent where
   readsPrec p b =
     case dropWhile (isSpace) b of
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
