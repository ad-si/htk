{- #########################################################################

MODULE        : DaVinciGraphTerm
AUTHOR        : Carla Blanck Purper, 
                Einar W. Karlsen 
                University of Bremen
                email:  {cpurper,ewk}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : Definition of DaVinci Graph Terms.

The type definitions are not yet decrypted.  However to do this should
not be too hard - try looking at the "Show" definitions and then
matching with the corresponding DaVinci commands.

   ######################################################################### -}


module DaVinciGraphTerm (
        GUIVALUE,

        AttrUpd(..),       
        AttrAssoc(..),     
        EdgeId(..),        
        EdgeDescr(..),     
        EdgeUpd(..),
        NodeId(..),        
        NodeDescr(..),     
        NodeUpd(..),
        TypeId(..),
 
        -- callDaVinci writes a DaVinci function call.
        callDaVinci,
        callDaVinciAcc,
        innerCallDaVinci,
        CallDaVinci(..),
        Arg(..)

        ) where

import GUIValue

import Debug(debug)




-- ---------------------------------------------------------------------------
-- Data Type Definitions
-- ---------------------------------------------------------------------------

data AttrUpd    = NodeAttrUpd NodeId [AttrAssoc]
                | EdgeAttrUpd EdgeId [AttrAssoc]

data AttrAssoc  = AttrAssoc String GUIVALUE 

newtype EdgeId  = EdgeId String deriving (Eq, Ord)

data EdgeDescr  = EdgeDescr EdgeId TypeId [AttrAssoc] NodeDescr

data EdgeUpd    = EdgeUpd EdgeId TypeId [AttrAssoc] NodeId NodeId
                | EdgeDel EdgeId

newtype NodeId  = NodeId String deriving (Eq, Ord)

data NodeDescr  = NodeDescr NodeId TypeId [AttrAssoc] [EdgeDescr]

data NodeUpd    = NodeUpd NodeId TypeId [AttrAssoc]
                | NodeDel NodeId

newtype TypeId  = TypeId String deriving (Eq, Ord)



-- ---------------------------------------------------------------------------
-- Unparsing Rules
-- ---------------------------------------------------------------------------

instance Show NodeId where
   showsPrec d (NodeId p) r = showsPrec d p r

instance Show EdgeId where
   showsPrec d (EdgeId p) r = showsPrec d p r

instance Show AttrUpd where
   showsPrec _ (NodeAttrUpd nid ats) = 
      callDaVinciAcc "node" [Arg nid,Arg ats]
   showsPrec _ (EdgeAttrUpd eid ats) =
      callDaVinciAcc "edge" [Arg eid,Arg ats]

instance Show AttrAssoc where
   showsPrec _ (AttrAssoc x y) =
      callDaVinciAcc "a" [Arg x,Arg y]
        
instance Show EdgeDescr where
   showsPrec _ (EdgeDescr eid tid ats et) =
      callDaVinciAcc "l" 
         [Arg eid,innerCallDaVinci "e" [Arg tid,Arg ats,Arg et]]

instance Show EdgeUpd where
   showsPrec _ (EdgeUpd eid1 tid ats nid1 nid2) =
      callDaVinciAcc "new_edge" [Arg eid1,Arg tid,Arg ats,Arg nid1,Arg nid2]
   showsPrec _ (EdgeDel eid1) =
      callDaVinciAcc "delete_edge" [Arg eid1]

instance Show NodeUpd where
   showsPrec _ (NodeUpd nid tid ats) =
      callDaVinciAcc "new_node" [Arg nid,Arg tid,Arg ats]
   showsPrec _ (NodeDel nid) = 
      callDaVinciAcc "delete_node" [Arg nid]

instance Show NodeDescr where
   showsPrec d (NodeDescr nid tid ats ets) =
      callDaVinciAcc "l" [Arg nid,
         innerCallDaVinci "n" [Arg tid,Arg ats,Arg ets]
         ]

instance Show TypeId where
   showsPrec _ (TypeId t) r = showsPrec 0 t r

-- ---------------------------------------------------------------------------
-- callDaVinci
-- callDaVinci and callDaVinciAcc unparses the standard DaVinci function call.
-- callDaVinciAcc allows you to add a parameter to append at the end.
--
-- Each parameter to the function call must be Show-able.  We build up
-- Show-definitions recursively.
--
-- innerCallDaVinci should be used for calls to DaVinci inside a call to
-- DaVinci.
-- ---------------------------------------------------------------------------

data Arg = forall item . Show item => Arg item
instance Show Arg where
   showsPrec prec (Arg item) toAppend = showsPrec prec item toAppend

callDaVinci :: String -> [Arg] -> String
callDaVinci funName funArgs = callDaVinciAcc funName funArgs ""
{-# INLINE callDaVinci #-}

callDaVinciAcc :: String -> [Arg] -> String -> String
callDaVinciAcc funName funArgs toAppend =
   let
      doOne (Arg item) toAppend = showsPrec 0 item toAppend

      withCommas [] toAppend = toAppend
      withCommas [one] toAppend = doOne one toAppend
      withCommas (first:rest) toAppend = 
         doOne first (',' : (withCommas rest toAppend))
   in
      (funName ++ "(") ++ (withCommas funArgs (')' : toAppend))
{-# INLINE callDaVinciAcc #-}

data CallDaVinci = CallDaVinci String [Arg]

instance Show CallDaVinci where
   showsPrec _ (CallDaVinci funName funArgs) toAppend =
      callDaVinciAcc funName funArgs toAppend

innerCallDaVinci :: String -> [Arg] -> Arg
innerCallDaVinci funName funArgs = Arg (CallDaVinci funName funArgs)

